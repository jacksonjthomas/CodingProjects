import mysql.connector 
import sys
import csv
from db_operations import db_operations
from helper import helper
from tabulate import tabulate

# connecting to a db
mydb = mysql.connector.connect(
    host = "localhost",
    user = "root",
    password = "BROtein69",
    database = "miniBaseballSavant"
)

db_ops = db_operations("./miniBaseballSavant.db")
mycursor = mydb.cursor() 

# loading data in 
# pitcherData = helper.data_cleaner("Datasets/pitchers.csv")
# hitterData = helper.data_cleaner("Datasets/hitters.csv")
# gameData = helper.data_cleaner("Datasets/games.csv")
# pxpData = helper.data_cleaner("Datasets/mlbpxpdata.csv")

def pre_process_table():
    # check if table exists 
    mycursor.execute('DROP TABLE IF EXISTS pxpData;')

    mycursor.execute('''CREATE TABLE pxpData (play_id INT NOT NULL, game_id INT NOT NULL, pitcher_id INT NOT NULL, pitcher VARCHAR(50), 
    throws VARCHAR(1), pitch_type VARCHAR(25), release_speed FLOAT(5,2), rel_side FLOAT(4,2), rel_height FLOAT(5,2), extension FLOAT(3,1), pfx_x FLOAT(4,2), pfx_y FLOAT(4,2), 
    spin_rate INT, spin_axis INT, balls INT, strikes INT, inning INT, inning_half VARCHAR(5), pitchCall VARCHAR(1), batter_id INT, launch_angle INT, exit_velo FLOAT(4,1), 
    distance INT, bb_type VARCHAR(25), events VARCHAR(250), away_team VARCHAR(3), home_team VARCHAR(3), post_away_score INT, post_home_score INT, 
    called_strike INT, inZone INT, swing INT, whiff INT, PRIMARY KEY(play_id))''')
    print("PXP TABLE CREATED") 

    # # check if table exists 
    mycursor.execute('DROP TABLE IF EXISTS pitchers;')

    mycursor.execute('''CREATE TABLE pitchers (pitcher_id INT NOT NULL , pitcher VARCHAR(50), throws VARCHAR(1), PRIMARY KEY(pitcher_id))''')
    print("PITCHER TABLE CREATED")

    # # check if table exists 
    mycursor.execute('DROP TABLE IF EXISTS hitters;')

    mycursor.execute('''CREATE TABLE hitters (hitter_id INT NOT NULL, hitter VARCHAR(50), PRIMARY KEY(hitter_id))''')
    print("HITTER TABLE CREATED")

    # check if table exists 
    mycursor.execute('DROP TABLE IF EXISTS games;')

    mycursor.execute('''CREATE TABLE games (game_id INT NOT NULL, game_date DATETIME, home_team VARCHAR(3), away_team VARCHAR(3), PRIMARY KEY(game_id))''')
    print("GAME TABLE CREATED") 

    # check if table exists 
    mycursor.execute('DROP TABLE IF EXISTS teams;')

    mycursor.execute('''CREATE TABLE teams (team_id int NOT NULL AUTO_INCREMENT, team VARCHAR(5), PRIMARY KEY(team_id))''')
    mycursor.execute('''ALTER TABLE teams AUTO_INCREMENT = 34500''')
    print("TEAM TABLE CREATED") 

    # check if table exists 
    mycursor.execute('DROP TABLE IF EXISTS pitches;')

    mycursor.execute('''CREATE TABLE pitches (pitch_id INT NOT NULL, game_id INT NOT NULL, pitcher_id INT NOT NULL, hitter_id INT NOT NULL, 
    pitch_type VARCHAR(25), release_speed FLOAT(5,2), rel_side FLOAT(4,2), rel_height FLOAT(5,2), extension FLOAT(3,1), pfx_x FLOAT(4,2), 
    pfx_y FLOAT(4,2), spin_rate INT, spin_axis INT, balls INT, strikes INT, pitchCall VARCHAR(1), called_strike INT, inZone INT, swing INT, whiff INT, PRIMARY KEY(pitch_id))''')
    print("PITCH TABLE CREATED") 

    # check if table exists 
    mycursor.execute('DROP TABLE IF EXISTS batted_ball_events;')

    mycursor.execute('''CREATE TABLE batted_ball_events (bbe_id INT NOT NULL AUTO_INCREMENT, pitch_id INT NOT NULL, game_id INT NOT NULL, hitter_id INT NOT NULL, launch_angle INT, exit_velo FLOAT(4,1), 
    distance INT, bb_type VARCHAR(25), events VARCHAR(250), PRIMARY KEY(bbe_id))''')
    mycursor.execute('''ALTER TABLE batted_ball_events AUTO_INCREMENT = 20000''')
    print("BBE TABLE CREATED") 

def pre_process_insert():
    # pxp data

    for row in pxpData:
        query = '''INSERT INTO miniBaseballSavant.pxpData(play_id, game_id, pitcher_id, pitcher, throws, pitch_type, release_speed, rel_side, rel_height,
        extension, pfx_x, pfx_y, spin_rate, spin_axis, balls, strikes, inning, inning_half, pitchCall, batter_id, launch_angle,
        exit_velo, distance, bb_type, events, away_team, home_team, post_away_score, post_home_score, called_strike, inZone, swing, whiff) 
        VALUES(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,NULLIF(%s,""),NULLIF(%s,""),NULLIF(%s,""),NULLIF(%s,""),NULLIF(%s,""),%s,%s,%s,%s,%s,%s,%s,%s)'''
        mycursor.execute(query, tuple(row))
        mydb.commit()
    print("GAWT IT - PXPData")

    # # pitchers data 

    for row in pitcherData:
        query = '''INSERT INTO miniBaseballSavant.pitchers(pitcher_id, pitcher, throws) VALUES(%s,%s,%s)'''
        mycursor.execute(query, tuple(row))
        mydb.commit()
    print("GAWT IT - PitcherData")

    # # # hitters data 

    for row in hitterData:
        query = '''INSERT INTO miniBaseballSavant.hitters(hitter_id, hitter) VALUES(%s,%s)'''
        mycursor.execute(query, tuple(row))
        mydb.commit()
    print("GAWT IT - HitterData")

    # games data 

    for row in gameData:
        query = '''INSERT INTO miniBaseballSavant.games(game_id, game_date, home_team, away_team) VALUES(%s,%s,%s,%s)'''
        mycursor.execute(query, tuple(row))
        mydb.commit()
    print("GAWT IT - GameData") 

    # team data 

    query = '''INSERT INTO teams(team) SELECT DISTINCT home_team FROM games'''
    mycursor.execute(query)
    mydb.commit()
    query = '''ALTER TABLE games add (home_team_id int NOT NULL, away_team_id int NOT NULL)'''
    mycursor.execute(query)
    query = '''UPDATE games INNER JOIN teams ON games.home_team = teams.team SET games.home_team_id = teams.team_id'''
    mycursor.execute(query)
    mydb.commit()
    query = '''UPDATE games INNER JOIN teams ON games.away_team = teams.team SET games.away_team_id = teams.team_id'''
    mycursor.execute(query)
    mydb.commit()
    query = '''ALTER TABLE games DROP COLUMN home_team, DROP COLUMN away_team'''
    mycursor.execute(query)
    mydb.commit()
    print("GAWT IT - Teams")

    # # pitch data 
    query = '''INSERT INTO pitches(pitch_id, game_id, pitcher_id, hitter_id, pitch_type, release_speed, rel_side, rel_height, extension, 
    pfx_x, pfx_y, spin_rate, spin_axis, balls, strikes, pitchCall, called_strike, inZone, swing, whiff) 
    SELECT play_id, game_id, pitcher_id, batter_id, pitch_type, release_speed, rel_side, rel_height, extension, 
    pfx_x, pfx_y, spin_rate, spin_axis, balls, strikes, pitchCall, called_strike, inZone, swing, whiff 
    FROM pxpData;'''
    mycursor.execute(query)
    mydb.commit()
    print("GAWT IT - Pitches")

    # # BBE data 
    query = '''INSERT INTO batted_ball_events(pitch_id, game_id, hitter_id, launch_angle, exit_velo, distance, bb_type, events)
    SELECT play_id, game_id, batter_id, launch_angle, exit_velo, distance, bb_type, events
    FROM pxpData;'''
    mycursor.execute(query)
    mydb.commit()
    print("GAWT IT - BBE")

    mycursor.execute('DROP TABLE IF EXISTS pxpData;')

def printPlayers():
    print('''
    Do you want to view all the Pitchers or Hitters in MLB?\n
    1. Pitcher
    2. Hitter
    ''')
    choice = helper.get_choice([1,2]) 
    if choice == 1:
        printPitchers()
    else:
        printHitters()

def printPitchers():
    mycursor.execute('DROP VIEW IF EXISTS MLBPitchers2022;')
    query = '''
    CREATE VIEW MLBPitchers2022 AS
        SELECT DISTINCT pitcher
        FROM pitchers
        ORDER BY pitcher;
    '''
    mycursor.execute(query)
    query = ''' 
    SELECT * FROM MLBPitchers2022
    '''
    mycursor.execute(query)
    myresult = mycursor.fetchall()
    field_names = [i[0] for i in mycursor.description]
    print(tabulate(myresult, headers = field_names, tablefmt = 'psql'))

def printHitters():
    mycursor.execute('DROP VIEW IF EXISTS MLBHitters2022;')
    query = '''
    CREATE VIEW MLBHitters2022 AS
        SELECT DISTINCT hitter
        FROM hitters
        ORDER BY hitter;
    '''
    mycursor.execute(query)
    query = ''' 
    SELECT * FROM MLBHitters2022
    '''
    mycursor.execute(query)
    myresult = mycursor.fetchall()
    field_names = [i[0] for i in mycursor.description]
    print(tabulate(myresult, headers = field_names, tablefmt = 'psql'))

def createTeam():
    print('''
    It's time for MLB Expansion! 
    ''')
    team = input("What is the name of your new MLB Franchise?\n")
    query = '''INSERT INTO teams(team) VALUES(%s)'''
    input_tuple = (team,)
    mycursor.execute(query,input_tuple)
    mydb.commit()

def updateTeam():
    print('''
    It's time for MLB Relocation! 
    ''')
    team = input("Which team is being relocated\n")
    query = '''SELECT * FROM teams WHERE team = %s'''
    input_tuple = (team,)
    mycursor.execute(query,input_tuple)
    results = mycursor.fetchall()
    id = str(results[0][0])

    try:
        newName = input("What new team name do you want?\n")
        query = 'UPDATE teams SET team = \''+newName+'\'  WHERE team_id = \''+id+'\''
        db_ops.update(query)
    except:
        print("Wrong syntax! Try again\n")
        print("Error: ", sys.exc_info())

    mydb.commit()

def deleteTeam():
    print("Oh no MLB has decided to cut a team from the league!\n")
    team = input("Which team got cut from MLB?\n")
    query = '''
    DELETE FROM teams 
    WHERE team = %s
    '''
    input_tuple = (team,)
    mycursor.execute(query,input_tuple)
    mydb.commit()

def compareHitters():
    hitterName1 = input("What hitter's stats do you want to see ?\n")
    hitterName2 = input("What hitter's metrics do you want to see ?\n")
    query = ''' 
    SELECT hitter as Hitter, COUNT(exit_velo) as BBE, ROUND(AVG(launch_angle), 2) as AvgLA, ROUND(AVG(exit_velo), 2) as AvgEV, MAX(exit_velo) as MaxEV,
    ROUND((COUNT(CASE WHEN exit_velo >= 95 THEN 1 END) / COUNT(bb_type)), 2) as HardHitRate,
    ROUND((COUNT(CASE WHEN bb_type = 'line_drive' THEN 1 END) / COUNT(bb_type)), 2) as LineDriveRate,
    ROUND((COUNT(CASE WHEN bb_type = 'fly_ball' THEN 1 END) / COUNT(bb_type)), 2) as FlyBallRate,
    ROUND((COUNT(CASE WHEN bb_type = 'ground_ball' THEN 1 END) / COUNT(bb_type)), 2) as GroundBallRate
    FROM batted_ball_events as bbe
    INNER JOIN hitters on bbe.hitter_id=hitters.hitter_id
    WHERE hitter = %s OR hitter = %s
    GROUP BY hitter
    ORDER BY hitter;
    '''
    input_tuple = (hitterName1, hitterName2)
    mycursor.execute(query, input_tuple)
    myresult = mycursor.fetchall()
    field_names = [i[0] for i in mycursor.description]
    print(tabulate(myresult, headers = field_names, tablefmt = 'psql'))

    writeToCSV(myresult, field_names)

def comparePitchers():
    pitcherName1 = input("What pitcher's pitches do you want to see ?\n")
    pitcherName2 = input("What other pitcher's pitches do you want to see ?\n")
    query = ''' 
    SELECT pitcher as Pitcher, pitch_type as PitchType, ROUND(AVG(release_speed), 2) as Velocity, COUNT(*) as Pitches, 
    ROUND(AVG(ABS(rel_side)), 2) as ReleaseSide, ROUND(AVG(rel_height), 2) as ReleaseHeight, ROUND(AVG(extension), 2) as Extension,
    ROUND(AVG(pfx_x), 2) as HorzBreak, ROUND(AVG(pfx_y), 2) as VertBreak, ROUND(AVG(spin_rate), 0) as SpinRate, 
    ROUND(AVG(spin_axis), 0) as SpinAxis, 
    ROUND((COUNT(CASE WHEN pitchCall = 'X' OR pitchCall = 'S' THEN 1 END) / COUNT(pitchCall)), 2) as StrikeRate, 
    ROUND(SUM(whiff)/SUM(swing), 2) as WhiffRate
    FROM pitches as p
    INNER JOIN pitchers ON p.pitcher_id=pitchers.pitcher_id
    WHERE pitcher = %s OR pitcher = %s
    GROUP BY pitch_type, pitcher
    ORDER BY pitch_type; 
    '''
    input_tuple = (pitcherName1, pitcherName2)
    mycursor.execute(query, input_tuple)
    myresult = mycursor.fetchall()
    field_names = [i[0] for i in mycursor.description]
    print(tabulate(myresult, headers = field_names, tablefmt = 'psql'))

    writeToCSV(myresult, field_names)

def matchup():
    print("Get stats of a hitter vs pitcher!\n")
    pitcher = input("What pitcher do you want?\n")
    hitter = input("What hitter do you want\n")
    query = ''' 
    SELECT pitcher as Pitcher, hitter as Hitter, COUNT(*) as Pitches,
    ROUND((COUNT(CASE WHEN exit_velo >= 95 THEN 1 END) / COUNT(bb_type)), 2) as HardHitRate,
    ROUND(SUM(whiff)/SUM(swing), 2) as WhiffRate
    FROM batted_ball_events as bbe
    JOIN hitters as h ON bbe.hitter_id=h.hitter_id
    JOIN pitches ON bbe.pitch_id=pitches.pitch_id
    JOIN pitchers ON pitches.pitcher_id=pitchers.pitcher_id 
    WHERE pitcher = %s AND hitter = %s;
    '''
    input_tuple = (pitcher, hitter)
    mycursor.execute(query, input_tuple)
    myresult = mycursor.fetchall()
    field_names = [i[0] for i in mycursor.description]
    print(tabulate(myresult, headers = field_names, tablefmt = 'psql'))

    writeToCSV(myresult, field_names)

def pitcherAppearances():
    pitcher = input("What pitcher's pitching appearances do you want to see?\n")
    query = '''
    SELECT DISTINCT Pitcher, date(Appearance) 
    FROM(
        SELECT pitcher as Pitcher, games.game_date as Appearance
        FROM pitches
        JOIN games ON pitches.game_id = games.game_id
        JOIN pitchers on pitches.pitcher_id = pitchers.pitcher_id
    ) sub
    WHERE pitcher = %s;
    '''
    input_tuple = (pitcher,)
    mycursor.execute(query, input_tuple)
    myresult = mycursor.fetchall()
    field_names = [i[0] for i in mycursor.description]
    print(tabulate(myresult, headers = field_names, tablefmt = 'psql'))

def writeToCSV(rows, field_names):
    fp = open('query.csv', 'w')
    myFile = csv.writer(fp)
    myFile.writerow(field_names)
    myFile.writerows(rows)
    fp.close

def userOptions():
    print('''
    Select from the following menu options:\n
    1) View Players\n 
    2) Create Team\n
    3) Update Team\n
    4) Delete Team\n
    5) Compare Pitchers\n
    6) Compare Hitters\n
    7) Pitcher vs Hitter Matchup\n
    8) Pitcher Appearances\n
    9) Exit MiniBaseballSavant!\n
    ''')
    return helper.get_choice([1,2,3,4,5,6,7,8,9]) 

# pre_process_table()
# pre_process_insert()
print("Welcome to MiniBaseballSavant!") 

# UIX
while True:
    match userOptions():
        case 1:
            printPlayers() # DONE
        case 2:
            createTeam() # DONE
        case 3:
            updateTeam() # DONE
        case 4:
            deleteTeam() # DONE
        case 5:
            comparePitchers() # DONE
        case 6: 
            compareHitters() # DONE
        case 7:
            matchup() 
        case 8: 
            pitcherAppearances() # DONE
        case 9: 
            print("Bye!")
            break
            
#close connection
db_ops.destructor()