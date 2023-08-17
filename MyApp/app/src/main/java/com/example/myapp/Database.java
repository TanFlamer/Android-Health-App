package com.example.myapp;

import android.content.Context;

import androidx.room.Room;
import androidx.room.RoomDatabase;

import com.example.myapp.databaseFiles.playlist.Playlist;
import com.example.myapp.databaseFiles.playlist.PlaylistDao;
import com.example.myapp.databaseFiles.sleep.Sleep;
import com.example.myapp.databaseFiles.sleep.SleepDao;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.databaseFiles.song.SongDao;
import com.example.myapp.databaseFiles.songcatalogue.SongCatalogue;
import com.example.myapp.databaseFiles.songcatalogue.SongCatalogueDao;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.sport.SportDao;
import com.example.myapp.databaseFiles.sportschedule.SportSchedule;
import com.example.myapp.databaseFiles.sportschedule.SportScheduleDao;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.type.TypeDao;
import com.example.myapp.databaseFiles.user.User;
import com.example.myapp.databaseFiles.user.UserDao;

@androidx.room.Database(entities = { User.class, Playlist.class, Song.class, Sleep.class, Sport.class, Type.class, SongCatalogue.class, SportSchedule.class },
                        version = 1,
                        exportSchema = false)
public abstract class Database extends RoomDatabase {

    //get database
    private static Database instance;

    //get user data access object
    public abstract UserDao getUserDao();

    //get playlist data access object
    public abstract PlaylistDao getPlaylistDao();

    //get song data access object
    public abstract SongDao getSongDao();

    //get sleep data data access object
    public abstract SleepDao getSleepDao();

    //get sport data data access object
    public abstract SportDao getSportDao();

    //get sport type data access object
    public abstract TypeDao getTypeDao();

    //get song catalogue data access object
    public abstract SongCatalogueDao getSongPlaylistDao();

    //get sport schedule data access object
    public abstract SportScheduleDao getTypeSportDao();

    //create and return single instance of database
    public static synchronized Database getInstance(Context context) {
        if (instance == null) {
            instance = Room.databaseBuilder(context.getApplicationContext(),
                            Database.class, "user_database")
                    .createFromAsset("database/user_database.db")
                    .build();
        }
        return instance;
    }
}
