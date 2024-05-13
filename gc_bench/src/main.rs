use gc::{GcHandle, Tracable, TraceCallback};

#[derive(Debug)]
struct Simple {
    f: [usize; 256],
}

impl Tracable for Simple {
    fn trace(&mut self, trace_fn: TraceCallback) {
        println!("tracing: {:?}", self)
    }
}

fn perform_work(gc: &mut GcHandle) {
    println!("initialized");
    let value = gc
        .alloc(Simple {
            f: [0xf0_f1_f2_f3_f4_f5_f6_f7; 256],
        })
        .unwrap();
    let cloned = value.clone();
    println!("{:?}, {:?}", value.as_raw(), cloned.as_raw());
    for _ in 0..100 {
        let value = gc.alloc(Simple { f: [1; 256] }).unwrap();
        println!("allocated {:?}", value.as_raw())
    }
    gc.force_collect();
    println!(
        "{:?}, {:?}, {:?}",
        value.as_raw(),
        cloned.as_raw(),
        gc.load(&value)
    );
}

fn main() {
    gc::with_gc(|gc| perform_work(gc)).unwrap();
}
