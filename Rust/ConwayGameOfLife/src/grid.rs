use crate::pattern::Pattern;

const DELTAS: [(i32, i32); 8] = [
    (-1, -1), // Top left
    (-1, 0),  // Top
    (-1, 1),  // Top Right
    (0, -1),  // Left
    (0, 1),   // Right
    (1, -1),  // Bottom left
    (1, 0),   // Bottom
    (1, 1),   // Bottom Right
];

pub struct Grid {
    pub w: usize,
    pub h: usize,
    pub items: Vec<bool>,
    pub prev: Vec<bool>,
    pub pattern: Pattern,
}

impl Grid {
    pub fn new(w: usize, h: usize, pattern: Pattern) -> Box<Grid> {
        let mut items = vec![false; w * h];
        for (x, y) in pattern.active_cell_coords() {
            items[y * w + x] = true;
        }
        Box::new(Grid { w, h, prev: items.clone(), items, pattern })
    }

    pub fn evolve(&mut self) {
        self.prev.clone_from(&self.items);
        let mut next = self.items.clone();

        let w = self.w as i32;
        let h = self.h as i32;

        for y in 0..self.h {
            let yi = y as i32;
            for x in 0..self.w {
                let xi = x as i32;

                let mut alive_neighbors = 0;
                for (dx, dy) in DELTAS {
                    let nx = xi + dx;
                    let ny = yi + dy;
                    if nx < 0 || nx >= w || ny < 0 || ny >= h {
                        continue;
                    }
                    let nidx = ny as usize * self.w + nx as usize;
                    if self.items[nidx] {
                        alive_neighbors += 1;
                    }
                }
                let i = y * self.w + x;
                next[i] = if self.items[i] {
                    alive_neighbors == 2 || alive_neighbors == 3
                } else {
                    alive_neighbors == 3
                };
            }
        }
        self.items = next;
    }

    // render 2 rows at a time
    pub fn as_halfblock_string(&self) -> String {
        let mut out = String::new();
        let h2 = self.h.div_ceil(2);
        for r in 0..h2 {
            let y_top = 2 * r;
            let y_bot = y_top + 1;
            for x in 0..self.w {
                let top = self.items[y_top * self.w + x];
                let bottom = if y_bot < self.h {
                    self.items[y_bot * self.w + x]
                } else {
                    false
                };
                let ch = match (top, bottom) {
                    (false, false) => ' ',
                    (true, false) => '▀',
                    (false, true) => '▄',
                    (true, true) => '█',
                };
                out.push(ch);
            }
            out.push('\n');
        }
        out
    }

    // pub fn as_string(&self) -> String {
    //     let mut out = String::new();
    //     out.push_str(&format!(
    //         "{:^width$}\n",
    //         self.pattern.name(),
    //         width = self.w
    //     ));
    //     for y in 0..self.h {
    //         for x in 0..self.w {
    //             let idx = y * self.w + x;
    //             out.push(if self.items[idx] { '█' } else { ' ' });
    //         }
    //         out.push('\n');
    //     }
    //     out
    // }
}
