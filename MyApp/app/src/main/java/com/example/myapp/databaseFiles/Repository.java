package com.example.myapp.databaseFiles;

import android.app.Application;

import androidx.room.Dao;

import com.example.myapp.databaseFiles.dao.PlaylistDao;
import com.example.myapp.databaseFiles.dao.SleepDao;
import com.example.myapp.databaseFiles.dao.SongDao;
import com.example.myapp.databaseFiles.dao.SportDao;
import com.example.myapp.databaseFiles.dao.TypeDao;
import com.example.myapp.databaseFiles.dao.UserDao;
import com.example.myapp.databaseFiles.entity.Playlist;
import com.example.myapp.databaseFiles.entity.Sleep;
import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.databaseFiles.entity.Sport;
import com.example.myapp.databaseFiles.entity.Type;
import com.example.myapp.databaseFiles.entity.User;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Repository {

    private UserDao userDao;
    private PlaylistDao playlistDao;
    private SongDao songDao;
    private SleepDao sleepDao;
    private SportDao sportDao;
    private TypeDao typeDao;

    private List<User> allUsers;
    private List<Playlist> allPlaylist;
    private List<Song> allSongs;
    private List<Sleep> allSleep;
    private List<Sport> allSport;
    private List<Type> allTypes;

    public Repository(Application application) {
        Database database = Database.getInstance(application);
        getAllData(database);
    }

    public void getAllData(Database database){
        userDao = database.getUserDao();
        allUsers = userDao.getAllUsers();
        playlistDao = database.getPlaylistDao();
        allPlaylist = playlistDao.getAllPlaylists();
        songDao = database.getSongDao();
        allSongs = songDao.getAllSongs();
        sleepDao = database.getSleepDao();
        allSleep = sleepDao.getAllSleep();
        sportDao = database.getSportDao();
        allSport = sportDao.getAllSport();
        typeDao = database.getTypeDao();
        allTypes = typeDao.getAllTypes();
    }

    private static class InsertExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private Dao dao;

        private InsertExecutorTask(Dao dao) {
            this.dao = dao;
        }

        protected void execute(User user){
            service.execute(() -> ((UserDao)dao).insert(user));
        }
        protected void execute(Playlist playlist){
            service.execute(() -> (UserDao)dao.insert(playlist));
        }
        protected void execute(Song song){
            service.execute(() -> (UserDao)dao.insert(song));
        }
        protected void execute(Sleep sleep){
            service.execute(() -> (UserDao)dao.insert(sleep));
        }
        protected void execute(Sport sport){
            service.execute(() -> (UserDao)dao.insert(sport));
        }
        protected void execute(Type type){
            service.execute(() -> (UserDao)dao.insert(type));
        }
    }
}
