use warplib::memory::WarpMemoryTrait;

#[implicit(warp_memory)]
fn read_from_memory(index: felt251) {
    warp_memory.read_u128(index);
}