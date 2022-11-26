package com.example.myapp;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.sqlite.db.SupportSQLiteDatabase;

import com.example.myapp.databaseFiles.playlist.PlaylistDao;
import com.example.myapp.databaseFiles.sleep.SleepDao;
import com.example.myapp.databaseFiles.song.SongDao;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylistDao;
import com.example.myapp.databaseFiles.sport.SportDao;
import com.example.myapp.databaseFiles.type.TypeDao;
import com.example.myapp.databaseFiles.typeSport.TypeSportDao;
import com.example.myapp.databaseFiles.user.UserDao;
import com.example.myapp.databaseFiles.playlist.Playlist;
import com.example.myapp.databaseFiles.sleep.Sleep;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylist;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.typeSport.TypeSport;
import com.example.myapp.databaseFiles.user.User;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@androidx.room.Database(entities = { User.class, Playlist.class, Song.class, Sleep.class, Sport.class, Type.class, SongPlaylist.class, TypeSport.class },
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
    public abstract SongPlaylistDao getSongPlaylistDao();
    public abstract TypeSportDao getTypeSportDao();

    public static synchronized Database getInstance(Context context) {
        if (instance == null) {
            instance = Room.databaseBuilder(context.getApplicationContext(),
                            Database.class, "user_database")
                    //.createFromAsset("user_database.db")
                    .addCallback(roomCallback)
                    .build();
        }
        return instance;
    }

    private static final Callback roomCallback = new Callback() {
        @Override
        public void onOpen(@NonNull SupportSQLiteDatabase db) {
            super.onOpen(db);
            //new PopulateDbExecutorTask(instance).execute();
        }
    };

    private static class PopulateDbExecutorTask{
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private UserDao userDao;
        PopulateDbExecutorTask(Database instance) {
            this.userDao = instance.getUserDao();
        }
        protected void execute(){
            //service.execute(() -> userDao.insert());
        }
    }
}
