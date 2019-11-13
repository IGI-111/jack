use inkwell::values::BasicValueEnum;
use std::collections::HashMap;

pub(super) struct ValueStore {
    vals: HashMap<String, BasicValueEnum>,
}

impl ValueStore {
    pub fn new(it: impl IntoIterator<Item = (String, BasicValueEnum)>) -> Self {
        let mut vals = HashMap::new();
        for (id, val) in it {
            vals.insert(id, val);
        }
        Self { vals }
    }
    pub fn extend_with_val(&self, id: String, val: BasicValueEnum) -> Self {
        let mut vals = self.vals.clone();
        vals.insert(id, val);
        Self { vals }
    }
    pub fn get(&self, id: &str) -> &BasicValueEnum {
        self.vals
            .get(id)
            .expect(&format!("No variable named {}", id))
    }
}
