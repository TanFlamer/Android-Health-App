package com.example.test.databaseFiles;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.sqlite.db.SupportSQLiteDatabase;

import com.example.test.databaseFiles.dao.PlaylistDao;
import com.example.test.databaseFiles.dao.SleepDao;
import com.example.test.databaseFiles.dao.SongDao;
import com.example.test.databaseFiles.dao.SportDao;
import com.example.test.databaseFiles.dao.TypeDao;
import com.example.test.databaseFiles.dao.UserDao;
import com.example.test.databaseFiles.entity.Playlist;
import com.example.test.databaseFiles.entity.Sleep;
import com.example.test.databaseFiles.entity.Song;
import com.example.test.databaseFiles.entity.Sport;
import com.example.test.databaseFiles.entity.Type;
import com.example.test.databaseFiles.entity.User;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@androidx.room.Database(entities = { User.class, Playlist.class, Song.class, Sleep.class, Sport.class, Type.class }, version = 1)
public abstract class Database extends RoomDatabase {

    private static Database instance;
    public abstract UserDao getUserDao();
    public abstract PlaylistDao getPlaylistDao();
    public abstract SongDao getSongDao();
    public abstract SleepDao getSleepDao();
    public abstract SportDao getSportDao();
    public abstract TypeDao getTypeDao();

    public static synchronized Database getInstance(Context context) {
        if (instance == null) {
            instance = Room.databaseBuilder(context.getApplicationContext(),
                            Database.class, "user_database")
                    //.createFromAsset("database/user_database.db")
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
