#[implicit(warp_memory)]
fn read_from_memory(index: felt252) {
    warp_memory.read_u128(index);
}
