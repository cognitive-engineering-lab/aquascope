use rustc_data_structures::{captures::Captures, fx::FxHashMap as HashMap};
use rustc_middle::mir::{BasicBlock, Location};

#[derive(Debug)]
pub struct MirOrderedLocations {
  pub(super) entry_block: Option<BasicBlock>,
  pub(super) exit_block: Option<BasicBlock>,
  // associated indices must remain sorted
  pub(super) locations: HashMap<BasicBlock, Vec<usize>>,
}

impl MirOrderedLocations {
  pub fn entry_location(&self) -> Option<Location> {
    self.entry_block.map(|block| {
      let statement_index = *self
        .locations
        .get(&block)
        .expect("Block with no associated locations")
        .first()
        .unwrap();
      Location {
        block,
        statement_index,
      }
    })
  }

  pub fn exit_location(&self) -> Option<Location> {
    let block = self.exit_block?;
    self.locations.get(&block).map_or_else(
      // Block has no associated index then default to the start
      || Some(block.start_location()),
      // Get the last associated index of the block
      |vs| {
        vs.last().map(|&statement_index| Location {
          block,
          statement_index,
        })
      },
    )
  }

  pub fn get_entry_exit_locations(&self) -> Option<(Location, Location)> {
    let entry = self.entry_location()?;
    let exit = self.exit_location()?;
    Some((entry, exit))
  }

  pub fn values(&self) -> impl Iterator<Item = Location> + Captures<'_> {
    self.locations.iter().flat_map(|(bb, idxs)| {
      idxs.iter().map(|idx| Location {
        block: *bb,
        statement_index: *idx,
      })
    })
  }
}
