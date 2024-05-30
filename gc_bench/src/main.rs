use gc::{specialized_types::string::SimpleGcString, GcHandle, GcPointer, GcResult, RawGcPointer};
use gc_derive::Trace;

#[derive(Debug, Trace)]
struct Simple {
    f: [usize; 256],
}

#[derive(Trace)]
struct Referencing {
    raw: RawGcPointer,
    other: GcPointer<SimpleGcString>,
    no_pointer: u16,
}

#[derive(Trace)]
struct ReferencingTup(u16, GcPointer<Referencing>);

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

    let refer = gc.alloc(Referencing {
        raw: value.as_raw().clone(),
        other: str.clone(),
        no_pointer: 42,
    })?;
    println!("refer: {:?}", refer.as_raw());

    gc.force_collect();
    println!(
        "{:?}, {:?}, {:?}",
        value.as_raw(),
        cloned.as_raw(),
        gc.load(&value)
    );

    println!("{:?}, {}", str.as_raw(), gc.load(&str).as_ref());
    println!("{:?}", refer.as_raw());
    let other = &gc.load(&refer).other;
    println!("{:?}, {}", other.as_raw(), gc.load(other).as_ref());

    Ok(())
}

fn main() {
    gc::with_gc(|gc| perform_work(gc)).unwrap().unwrap();
}

#[derive(Trace)]
enum AllocEnum {
    A(usize, i32),
    B { f: usize },
}

#[derive(Trace)]
enum AllocEnumSimple {
    VarA,
    VarB,
}
