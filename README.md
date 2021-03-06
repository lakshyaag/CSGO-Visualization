# CSGO Visualization

Library to quickly view **grenade trajectories** and **weapon heatmaps** for all players by parsing CSGO demo files.

## Table of contents

- [CSGO Visualization](#csgo-visualization)
  - [Table of contents](#table-of-contents)
  - [Setup](#setup)
    - [Prerequisities](#prerequisities)
    - [Installation](#installation)
  - [Directory structure](#directory-structure)
  - [Example](#example)
  - [Questions](#questions)
    - [What if I want to parse multiple matches on the same map?](#what-if-i-want-to-parse-multiple-matches-on-the-same-map)
  - [Contributing](#contributing)
  - [Contact](#contact)
  - [Acknowledgments](#acknowledgments)
  - [`parse.py`](#parsepy)

---

## Setup

### Prerequisities

In order to parse the `.dem` files, you will need to install the CS:GO Python package available **[here](https://github.com/pnxenopoulos/csgo).**

### Installation

After installing the CS:GO library on your environment and downloading the required demo(s):

1. Clone this repo by running `git clone https://github.com/lakshyaag/CSGO-Visualization`.
2. Change the working directory to the newly cloned directory
3. Create a new folder called `demos` and move all download `.dem` files to be parsed - read [Directory Strucutre](#directory-structure) for more
4. Run `parse.py`, specifying the folder name containing the `.dem` files and the `match_id`
5. The parser will take care of appending all maps in a match and will provide `.csv`/`.xlsx` files for multiple metrics such as Kills, Damages, Grenades, Rounds, etc. in a folder named `parsed_demo_files`
6. Upload the `grenades.csv`/`weapons.csv` file of the respective match to the [Shiny app](https://lakshyaag.shinyapps.io/CSGO-Visualization/)!

## Directory structure

```dir
.
├── CSGO-Visualization
│   ├── demos                     # Parent folder containing all match folders
|   |   ├── match_1               # .dem files for Match #1
|   |   |   ├── demo_1_1          # .dem file for Map #1 Match #1
|   |   |   ├── demo_1_2          # .dem file for Map #2 Match #1
|   |   |   ├── demo_1_3          # .dem file for Map #3 Match #1
|   |   ├── match_2               # .dem files for Match #2
|   |   |   ├── demo_2_1          # .dem file for Map #1 Match #2
|   |   |   ├── demo_2_2          # .dem file for Map #2 Match #2
|   |   ├── match_3               # .dem files for Match #3
|   |   |   ├── demo_3_1          # .dem file for Map #1 Match #3
├── maps                          # Contains map images
```

## Example

Using this parser is simple. For example, if you wanted to check the grenade trajectories for [this match](https://www.hltv.org/matches/2352507/copenhagen-flames-vs-nip-pgl-major-stockholm-2021), you would:

1. Download the `.zip` file from HLTV and unzip the contents into `/CSGO-Visualization/demos/PGL-Major-Stockholm-2021-copenhagen-flames-vs-nip-bo3`
2. Open a terminal/shell and navigate to `/CSGO-Visualization/demos/`
3. Run the following line

   ```cmd
   python ../parse.py "./PGL-Major-Stockholm-2021-copenhagen-flames-vs-nip-bo3/" "cph-vs-nip-bo3-pgl-major-2021" -j
   ```

   Here, the first argument specifies the folder containing the `.dem` files, the second argument provides the `match_id` to use. Finally, `-j` tells the parser to remove the interim `.json` output to save disk space.

4. Once the parser has completed processing, the files will be stored in `/CSGO-Visualization/demos/parsed_demo_files/cph-vs-nip-bo3-pgl-major-2021/`
5. From here, simply upload the `g2-vs-nip-bo3-pgl-major-2021_grenades_overall.xlsx` to the [Shiny application](https://lakshyaag.shinyapps.io/CSGO-Visualization/)

## Questions

### What if I want to parse multiple matches on the same map?

That's easy to do. Simply put all of the `.dem` files that you wish to parse in the same folder and pass that folder as an argument to the parser.

For example, if you wish to the grenade trajectories for all matches played on *de_overpass* (with the ability to filter by player), you can move all the *de_overpass* `.dem` files in a folder `/demos/ovp_demos` and pass `ovp_demos` to the parser.

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## Contact

Please feel free to reach out to me on [Twitter](https://twitter.com/lakshyaag)

## Acknowledgments

Huge thanks to the Python [csgo](https://github.com/pnxenopoulos/csgo) package for efficient parsing, to [SimpleRadar](https://readtldr.gg/simpleradar-download) for the cleanest radar images available and to [HLTV](https://www.hltv.org/) for being the go-to source of Counter-Strike.

## `parse.py`

```cmd
python .\parse.py -h

usage: parse.py [-h] [-j] path/to/demo match_id

Parse .dem files in folder provided

positional arguments:
  path/to/demo       Path to folder containing .dem file(s)
  match_id           Match ID to use for filenames

optional arguments:
  -h, --help         show this help message and exit
  -j, --json_remove  Flag to remove .json generated by parser
  ```
