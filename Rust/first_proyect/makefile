.PHONY: doc

doc:
	if exist doc rd /s /q doc && rustdoc src\lib.rs --crate-name docs && start .\doc\docs\index.html
run:
	cargo clean && cargo build && cargo run
test:
	cargo clean && cargo build && cargo test
clean:
	if exist Cargo.lock del /f Cargo.lock
	cargo clean
	if exist doc rd /s /q doc