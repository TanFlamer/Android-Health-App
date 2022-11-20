package com.example.myapp.databaseFiles;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.sqlite.db.SupportSQLiteDatabase;

import com.example.myapp.databaseFiles.dao.PlaylistDao;
import com.example.myapp.databaseFiles.dao.SleepDao;
import com.example.myapp.databaseFiles.dao.SongDao;
import com.example.myapp.databaseFiles.dao.SongPlaylistDao;
import com.example.myapp.databaseFiles.dao.SportDao;
import com.example.myapp.databaseFiles.dao.TypeDao;
import com.example.myapp.databaseFiles.dao.TypeSportDao;
import com.example.myapp.databaseFiles.dao.UserDao;
import com.example.myapp.databaseFiles.entity.Playlist;
import com.example.myapp.databaseFiles.entity.Sleep;
import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.databaseFiles.entity.SongPlaylist;
import com.example.myapp.databaseFiles.entity.Sport;
import com.example.myapp.databaseFiles.entity.Type;
import com.example.myapp.databaseFiles.entity.TypeSport;
import com.example.myapp.databaseFiles.entity.User;

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
