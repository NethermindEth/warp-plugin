use warp::memory::WarpMemoryTrait;

#[warp_memory]
fn read_from_memory(index: felt251) {
    warp_memory.read_u128(index);
}