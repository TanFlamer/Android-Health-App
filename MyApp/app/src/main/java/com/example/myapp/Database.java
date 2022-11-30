package com.example.myapp;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.sqlite.db.SupportSQLiteDatabase;

import com.example.myapp.databasefiles.playlist.PlaylistDao;
import com.example.myapp.databasefiles.sleep.SleepDao;
import com.example.myapp.databasefiles.song.SongDao;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogueDao;
import com.example.myapp.databasefiles.sport.SportDao;
import com.example.myapp.databasefiles.type.TypeDao;
import com.example.myapp.databasefiles.sportschedule.SportScheduleDao;
import com.example.myapp.databasefiles.user.UserDao;
import com.example.myapp.databasefiles.playlist.Playlist;
import com.example.myapp.databasefiles.sleep.Sleep;
import com.example.myapp.databasefiles.song.Song;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogue;
import com.example.myapp.databasefiles.sport.Sport;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.databasefiles.sportschedule.SportSchedule;
import com.example.myapp.databasefiles.user.User;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

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
                    .addCallback(roomCallback)
                    .build();
        }
        return instance;
    }

    private static final Callback roomCallback = new Callback() {
        @Override
        public void onCreate(@NonNull SupportSQLiteDatabase db) {
            super.onCreate(db);
            new PopulateDbExecutorTask(instance).execute();
        }
    };

    private static class PopulateDbExecutorTask{
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final UserDao userDao;
        PopulateDbExecutorTask(Database instance) {
            this.userDao = instance.getUserDao();
        }
        protected void execute(){
            service.execute(() -> userDao.insert(new User(0, "GUEST", "")));
            service.execute(() -> userDao.insert(new User(-1, "NEW USER", "")));
        }
    }
}
