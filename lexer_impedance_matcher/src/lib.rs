#![cfg_attr(not(test), no_std)]

mod buffer;

use core::{
    future::Future,
    pin::{pin, Pin},
    task::{Context, Poll, RawWaker, RawWakerVTable, Waker},
};

use buffer::Buffer;

pub struct ImpedanceMatcher<T> {
    data: Buffer<64, T>,
}

/// A simple future that will resolve after being polled once
pub struct PushFuture {
    ready: bool,
}

impl Future for PushFuture {
    type Output = ();

    fn poll(
        self: core::pin::Pin<&mut Self>,
        _cx: &mut core::task::Context<'_>,
    ) -> core::task::Poll<Self::Output> {
        if self.ready {
            core::task::Poll::Ready(())
        } else {
            self.get_mut().ready = true;
            Poll::Pending
        }
    }
}

impl<T> ImpedanceMatcher<T> {
    pub fn push(&self, item: T) -> PushFuture {
        let ready = self.data.push(item);
        PushFuture { ready }
    }
    pub fn new() -> Self {
        Self {
            data: Buffer::new(),
        }
    }
    fn next(&self) -> Option<T> {
        self.data.read()
    }

    pub fn run<TFutRes, TRes, FRes>(
        &self,
        generator: impl Future<Output = TFutRes>,
        consumer: FRes,
    ) -> (Option<TFutRes>, TRes)
    where
        FRes: FnOnce(IteratorAdapter<'_, '_, T, TFutRes>) -> TRes,
    {
        // infra
        let waker = build_dummy_waker();
        let context = Context::from_waker(&waker);

        let future = pin!(generator);
        let future: Pin<&mut dyn Future<Output = _>> = future;
        let mut generator_result = None;
        let iter = IteratorAdapter {
            context,
            matcher: &self,
            future_output: &mut generator_result,
            future,
        };
        let result = consumer(iter);
        (generator_result, result)
    }
}

pub struct IteratorAdapter<'a, 'fut_out, T, TFRes> {
    matcher: &'a ImpedanceMatcher<T>,
    future: Pin<&'a mut dyn Future<Output = TFRes>>,
    future_output: &'fut_out mut Option<TFRes>,
    context: Context<'a>,
}

impl<T, TFRes> Iterator for IteratorAdapter<'_, '_, T, TFRes> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.matcher.next().or_else(|| {
            if self.future_output.is_none() {
                let pinned = pin!(&mut self.future);
                if let Poll::Ready(v) = pinned.poll(&mut self.context) {
                    *self.future_output = Some(v);
                }
            }
            self.matcher.next()
        })
    }
}

fn build_dummy_waker() -> Waker {
    const VTABLE: RawWakerVTable = RawWakerVTable::new(
        |_ptr| unreachable!(),
        |_ptr| unreachable!(),
        |_ptr| unreachable!(),
        |_ptr| {},
    );
    unsafe {
        let raw = RawWaker::new(core::ptr::null(), &VTABLE);
        Waker::from_raw(raw)
    }
}

#[cfg(test)]
#[test]
fn run_full_test() {
    async fn generate_outputs(matcher: &ImpedanceMatcher<u32>) {
        println!("gen_outputs");
        matcher.push(0).await;
        println!("after pushing 0");
        matcher.push(1).await;
        println!("after pushing 1");
        matcher.push(2).await;
    }

    let matcher = ImpedanceMatcher::new();
    let (gen_out, res) = matcher.run(generate_outputs(&matcher), |iter| {
        let mut res = Vec::new();
        for item in iter {
            println!("received {item}");
            res.push(item);
        }
        res
    });
    assert!(gen_out.is_some());

    assert_eq!([0, 1, 2].as_ref(), &res);
}
