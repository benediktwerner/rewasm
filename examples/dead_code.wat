(module
    (func (param i32 i32) (result i32) (local i32 i32 i32 i32)
        i32.const 42
        set_local 4
        block
            loop
                get_local 0
                i32.const 0
                i32.le_s
                br_if 1

                get_local 1
                set_local 2

                block
                    loop
                        get_local 2
                        i32.const 0
                        i32.le_s
                        br_if 1

                        get_local 3
                        i32.const 1
                        i32.add
                        set_local 3

                        get_local 2
                        i32.const -1
                        i32.add
                        set_local 2

                        get_local 4
                        i32.const 2
                        i32.mul
                        set_local 4

                        br 0
                    end
                end

                get_local 0
                i32.const -1
                i32.add
                set_local 0
                br 0
            end
        end
        get_local 3
    )
)
