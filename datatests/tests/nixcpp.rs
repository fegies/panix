use std::{cmp::Ordering, path::Path};

use interpreter::Evaluator;

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

fn parse_ok_test(path: &Path) -> datatest_stable::Result<()> {
    let mut expected_path = path.to_owned();
    expected_path.set_extension("exp");

    let expected_file_cont = std::fs::read(&expected_path)?;
    let file_cont = std::fs::read(path)?;

    let expected = parser::parse_nix(&expected_file_cont).unwrap();
    let value = parser::parse_nix(&file_cont).unwrap();

    // assert_eq!(expected, value);

    Ok(())
}

datatest_stable::harness! {
    {
        test = eval_ok_test,
        root = "/scratch/nix/tests/functional/lang",
        pattern = r"^eval-ok.*\.nix$",
    },
    {
        test = parse_ok_test,
        root = "/scratch/nix/tests/functional/lang",
        pattern = r"^parse-ok.*\.nix$",
    }
}
