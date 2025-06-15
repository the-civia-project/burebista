#[cfg(test)]
mod simple {
    use crate::parser::transform;

    #[test]
    fn static_var_declarations() {
        let code = transform(
            r#"
             const a: string = "42";
             const b: number = 42.42;
             const c: boolean = false;
            "#,
            None,
        )
        .unwrap();

        assert_eq!(
            code,
            r#"static a: String = String::from("42");
static b: f64 = 42.42;
static c: bool = false;
"#
        );
    }

    #[test]
    fn functions() {
        let code = transform(
            r#"
                function foo(): number { return 0; }
                export function add(a: number, b: number): number { let sum: number = a + b; return sum; }
            "#,
            None,
        )
        .unwrap();

        assert_eq!(
            code,
            r#"fn foo() -> f64 {
    return 0;
}
pub fn add(a: f64, b: f64) -> f64 {
    let sum: f64 = a + b;
    return sum;
}
"#
        );
    }
}
