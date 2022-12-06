package com.example.myapp;

import android.content.Context;

import androidx.room.Room;
import androidx.room.RoomDatabase;

import com.example.myapp.databasefiles.playlist.Playlist;
import com.example.myapp.databasefiles.playlist.PlaylistDao;
import com.example.myapp.databasefiles.sleep.Sleep;
import com.example.myapp.databasefiles.sleep.SleepDao;
import com.example.myapp.databasefiles.song.Song;
import com.example.myapp.databasefiles.song.SongDao;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogue;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogueDao;
import com.example.myapp.databasefiles.sport.Sport;
import com.example.myapp.databasefiles.sport.SportDao;
import com.example.myapp.databasefiles.sportschedule.SportSchedule;
import com.example.myapp.databasefiles.sportschedule.SportScheduleDao;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.databasefiles.type.TypeDao;
import com.example.myapp.databasefiles.user.User;
import com.example.myapp.databasefiles.user.UserDao;

@androidx.room.Database(entities = { User.class, Playlist.class, Song.class, Sleep.class, Sport.class, Type.class, SongCatalogue.class, SportSchedule.class },
                        version = 1,
                        exportSchema = false)
public abstract class Database extends RoomDatabase {

    private static Database instance;
    public abstract UserDao getUserDao();
    public abstract PlaylistDao getPlaylistDao();
    public abstract SongDao getSongDao();
    public abstract SleepDao getSleepDao();
    public abstract SportDao getSportDao();
    public abstract TypeDao getTypeDao();
    public abstract SongCatalogueDao getSongPlaylistDao();
    public abstract SportScheduleDao getTypeSportDao();

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
