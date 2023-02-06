# module defines operations to use with sqlite3 database
import mysql.connector 

# connecting to a db
mydb = mysql.connector.connect(
    host = "localhost",
    user = "root",
    password = "BROtein69",
    database = "miniBaseballSavant"
)


class db_operations():

    def __init__(self,conn_path): # constructor with connection path to db
        self.connection = mydb
        self.cursor = self.connection.cursor()
        print("connection made..")

    # function for bulk inserting records
    def bulk_insert(self,query,records):
        self.cursor.executemany(query,records)
        self.connection.commit()
        print("query executed..")

    # function to return a single value from table
    def single_record(self,query):
        self.cursor.execute(query)
        return self.cursor.fetchone()[0]

    # function to return a single attribute values from table
    def single_attribute(self,query):
        self.cursor.execute(query)
        results = self.cursor.fetchall()
        results = [i[0] for i in results]
        #results.remove(None)
        return results

    # function to print out all the attributes of a song to a user
    def print_row(self,query):
        self.cursor.execute(query)
        results = self.cursor.fetchall()
        return results

    # SELECT with named placeholders
    def name_placeholder_query(self,query,dictionary):
        self.cursor.execute(query,dictionary)
        results = self.cursor.fetchall()
        results = [i[0] for i in results]
        return results

    def update(self, query):
        self.cursor.execute(query)
        self.connection.commit()
        return
        query = '''
        CREATE TABLE songs(
            songID VARCHAR(22) NOT NULL PRIMARY KEY,
            Name VARCHAR(20),
            Artist VARCHAR(20),
            Album VARCHAR(20),
            releaseDate DATETIME,
            Genre VARCHAR(20),
            Explicit BOOLEAN,
            Duration DOUBLE,
            Energy DOUBLE,
            Danceability DOUBLE,
            Acousticness DOUBLE,
            Liveness DOUBLE,
            Loudness DOUBLE
        );
        '''

        self.cursor.execute(query)
        print('Table Created')

    # close connection
    def destructor(self):
        self.connection.close()
