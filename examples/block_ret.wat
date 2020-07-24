(module
    (func (param i32 i32) (result i32)
        get_local 0
        get_local 1
        i32.ge_u
        if (result i32)
            get_local 0
        else
            get_local 1
        end
        return
    )
)
