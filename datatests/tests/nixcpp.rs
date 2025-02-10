use std::{cmp::Ordering, path::Path};

use interpreter::{evaluator, Evaluator};

fn eval_ok_test(path: &Path) -> datatest_stable::Result<()> {
    gc::with_gc(|handle| {
        let mut expected_path = path.to_owned();
        expected_path.set_extension("exp");
        let expected_thunk = interpreter::compile_file(handle, &expected_path).unwrap();
        let thunk = interpreter::compile_file(handle, path).unwrap();

        let mut evaluator = Evaluator::new(handle).unwrap();
        let expected_value = evaluator.eval_expression(expected_thunk).unwrap();
        let obtained_value = evaluator.eval_expression(thunk).unwrap();

        assert_eq!(
            Some(Ordering::Equal),
            evaluator
                .compare_values(&expected_value, &obtained_value)
                .unwrap()
        )
    })
    .unwrap();
    Ok(())
}

datatest_stable::harness! {
    {
        test = eval_ok_test,
        root = "/scratch/nix/tests/functional/lang",
        pattern = r"^eval-ok.*\.nix$",
    },
}
