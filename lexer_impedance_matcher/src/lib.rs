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

    pub fn run<TFut, TRes, FRes>(&self, generator: TFut, consumer: FRes) -> TRes
    where
        FRes: for<'a> FnOnce(IteratorAdapter<'a, T, TFut>) -> TRes,
    {
        // infra
        let waker = build_dummy_waker();
        let context = Context::from_waker(&waker);

        let future = pin!(generator);
        let iter = IteratorAdapter {
            context,
            matcher: &self,
            future,
            completed: false,
        };
        consumer(iter)
    }
}

pub struct IteratorAdapter<'a, T, F> {
    matcher: &'a ImpedanceMatcher<T>,
    future: Pin<&'a mut F>,
    context: Context<'a>,
    completed: bool,
}

impl<T, F> Iterator for IteratorAdapter<'_, T, F>
where
    F: Future,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.matcher.next().or_else(|| {
            if !self.completed {
                let pinned = pin!(&mut self.future);
                if let Poll::Ready(_) = pinned.poll(&mut self.context) {
                    self.completed = true;
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
    let res = matcher.run(generate_outputs(&matcher), |iter| {
        let mut res = Vec::new();
        for item in iter {
            println!("received {item}");
            res.push(item);
        }
        res
    });

    assert_eq!([0, 1, 2].as_ref(), &res);
}
