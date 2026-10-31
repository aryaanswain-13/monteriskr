# Sample data for the demos

| File | What it is | Licence |
|---|---|---|
| `fest_past.csv`, `fest_plan.csv` | Synthetic college-fest data generated for the demos. Not real events. | MIT, same as the package |
| `cfb_ark.csv` | Arkansas football games, 2000-2018, extracted from a third-party dataset (see below). | CC BY-SA 4.0 |

## Attribution for `cfb_ark.csv`

Derived from **College Football Games (2000 to 2018)** by Jeff Gallini,
<https://www.kaggle.com/datasets/jeffgallini/college-football-attendance-2000-to-2018>,
licensed under
[CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/).

**Changes made:** the original file covers many teams. Only Arkansas rows were
kept, and the columns were reduced and renamed to `attendance`, `capacity`,
`ranked`, `opp_ranked`, `wins`, `tmax`, `tv` and `conference_game`. The
`ranked`, `opp_ranked` and `tv` columns are TRUE/FALSE flags derived from the
original ranking and TV text columns. The other values are unchanged.

`cfb_ark.csv` is shared under the same licence (CC BY-SA 4.0). The licence applies
to this data file only; the rest of the repository is under the package's MIT licence.
The data files are not part of the installed R package.
