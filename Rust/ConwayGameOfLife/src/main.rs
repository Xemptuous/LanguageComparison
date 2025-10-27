extern crate pancurses;

mod grid;
mod pattern;

use std::{thread, time::Duration};

use clap::Parser;
use grid::Grid;
use pancurses::*;
use pattern::Pattern;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long, value_enum, num_args = 1..)]
    pattern: Option<Vec<Pattern>>,
    #[arg(short, long, default_value = "30")]
    gens: usize,
    #[arg(long, default_value = "40")]
    width: usize,
    #[arg(long, default_value = "40")]
    height: usize,
    #[arg(short, long, default_value = "7")]
    framerate: u64,
}

fn main() {
    let cli = Cli::parse();

    let win = initscr();
    noecho();
    curs_set(0);
    start_color();
    use_default_colors();

    if let Some(patterns) = cli.pattern {
        for pattern in patterns {
            let mut grid = Grid::new(cli.width, cli.height, pattern);
            let title = format!("{:^width$}", grid.pattern.name(), width = grid.w);

            for _ in 0..=cli.gens {
                win.erase();
                win.mvaddstr(0, 0, &title);
                win.mv(1, 0);
                win.addstr(grid.as_halfblock_string());
                win.refresh();
                thread::sleep(Duration::from_millis(1000 / cli.framerate));
                grid.evolve();
            }
        }
    } else {
        for pattern in Pattern::all() {
            let mut grid = Grid::new(cli.width, cli.height, *pattern);
            let title = format!("{:^width$}", grid.pattern.name(), width = grid.w);

            for _ in 0..=cli.gens {
                win.erase();
                win.mvaddstr(0, 0, &title);
                win.mv(1, 0);
                win.addstr(grid.as_halfblock_string());
                win.refresh();
                thread::sleep(Duration::from_millis(1000 / cli.framerate));
                grid.evolve();
            }
        }
    }

    endwin();
}
