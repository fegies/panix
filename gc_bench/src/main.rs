use gc::{GcHandle, GcResult, Tracable, TraceCallback};

#[derive(Debug)]
struct Simple {
    f: [usize; 256],
}

impl Tracable for Simple {
    fn trace(&mut self, trace_fn: TraceCallback) {}
}

fn perform_work(gc: &mut GcHandle) -> GcResult<()> {
    println!("initialized");
    let value = gc
        .alloc(Simple {
            f: [0xf0_f1_f2_f3_f4_f5_f6_f7; 256],
        })
        .unwrap();
    let cloned = value.clone();

    let str = gc.alloc_string("a test string")?;
    println!("str: {:?}, {}", str.as_raw(), gc.load(&str).as_ref());

    println!("{:?}, {:?}", value.as_raw(), cloned.as_raw());

    for _ in 0..100 {
        let _value = gc.alloc(Simple { f: [1; 256] }).unwrap();
    }
    gc.force_collect();
    println!(
        "{:?}, {:?}, {:?}",
        value.as_raw(),
        cloned.as_raw(),
        gc.load(&value)
    );

    println!("{:?}, {}", str.as_raw(), gc.load(&str).as_ref());

    Ok(())
}

fn main() {
    gc::with_gc(|gc| perform_work(gc)).unwrap().unwrap();
}
