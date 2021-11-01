import argparse
import pandas as pd
import os
from pathlib import Path

from csgo.parser import DemoParser


def getDemoFiles(folder):
    files = [f"{folder}/{file}" for file in os.listdir(f"{folder}/")]
    return(files)


def parseDemo(demofile):

    print(f"Parsing: {demofile}")

    parser = DemoParser(demofile=demofile,
                        parse_rate=128)

    match_data = parser.parse(return_type='df')

    return({"rounds": match_data['rounds'],
            "kills": match_data['kills'],
           "grenades": match_data['grenades'],
            "damages": match_data['damages'],
            "flashes": match_data['flashes'],
            "bomb_events": match_data['bombEvents'],
            "playerFrames": match_data['playerFrames']})


def appendData(match_data):
    match_rounds = pd.DataFrame()
    match_kills = pd.DataFrame()
    match_grenades = pd.DataFrame()
    match_damages = pd.DataFrame()
    match_flashes = pd.DataFrame()
    match_bomb_events = pd.DataFrame()
    match_frames = pd.DataFrame()

    for map_ in match_data:
        match_rounds = match_rounds.append(map_['rounds'])
        match_kills = match_kills.append(map_['kills'])
        match_grenades = match_grenades.append(map_['grenades'])
        match_damages = match_damages.append(map_['damages'])
        match_flashes = match_flashes.append(map_['flashes'])
        match_bomb_events = match_bomb_events.append(map_['bomb_events'])
        match_frames = match_frames.append(map_['playerFrames'])

    return({
        'rounds': match_rounds,
        'kills': match_kills,
        'grenades': match_grenades,
        'damages': match_damages,
        'flashes': match_flashes,
        'bomb_events': match_bomb_events,
        'playerFrames': match_frames})


def saveData(match_df, id_):

    Path(f"./parsed_demo_files/{id_}").mkdir(parents=True, exist_ok=True)

    match_df['rounds'].to_csv(
        f"./parsed_demo_files/{id_}/{id_}_rounds.csv", index=False)
    match_df['kills'].to_csv(
        f"./parsed_demo_files/{id_}/{id_}_kills.csv", index=False)
    match_df['grenades'].to_csv(
        f"./parsed_demo_files/{id_}/{id_}_grenades.csv", index=False)
    match_df['damages'].to_csv(
        f"./parsed_demo_files/{id_}/{id_}_damages.csv", index=False)
    match_df['flashes'].to_csv(
        f"./parsed_demo_files/{id_}/{id_}_flashes.csv", index=False)
    match_df['bomb_events'].to_csv(
        f"./parsed_demo_files/{id_}/{id_}_bomb_events.csv", index=False)
    match_df['playerFrames'].to_csv(
        f"./parsed_demo_files/{id_}/{id_}_playerFrames.csv", index=False)


def removeJson():
    [os.remove(f) for f in os.listdir('./') if f.endswith(".json")]


def parseMatch(folder, id_, removeJsonFlag):

    demo_files = getDemoFiles(folder)
    demo_dfs = [parseDemo(file) for file in demo_files]

    match_df = appendData(demo_dfs)

    saveData(match_df, id_)

    print(f"Parsed demos for {id_}.")

    if(removeJsonFlag):
        print("Deleting .json files...")
        removeJson()

    return True


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Parse .dem files in folder provided")
    parser.add_argument('path', metavar="path/to/demo",
                        type=str, help="Path to folder containing .dem file(s)")
    parser.add_argument('id_', metavar="match_id", type=str,
                        help="Match ID to use for filenames")

    parser.add_argument('-j', '--json_remove', action='store_true',
                        help="Flag to remove .json generated by parser")

    args = parser.parse_args()
    
    parseMatch(folder=args.path, id_=args.id_, removeJsonFlag=args.json_remove)
