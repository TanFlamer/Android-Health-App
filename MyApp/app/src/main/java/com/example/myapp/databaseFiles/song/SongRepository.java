package com.example.myapp.databasefiles.song;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.Database;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SongRepository {

    //song data access object
    private final SongDao songDao;

    //constructor for song repository
    public SongRepository(Application application) {
        Database database = Database.getInstance(application);
        songDao = database.getSongDao();
    }

    //insert operation for song repository
    public void insert(Song song) {
        new InsertSongExecutorTask(songDao).execute(song);
    }

    //update operation for song repository
    public void update(Song song) {
        new UpdateSongExecutorTask(songDao).execute(song);
    }

    //delete operation for song repository
    public void delete(Song song) {
        new DeleteSongExecutorTask(songDao).execute(song);
    }

    //check if song with specific name exists for a user
    public Song findSong(int userID, String songName) {
        return new FindSongExecutorTask(songDao).find(userID, songName);
    }

    //returns live data of all songs belonging to a user
    public LiveData<List<Song>> getAllSongs(int userID) {
        return songDao.getAllSongs(userID);
    }

    //insert song executor task
    private static class InsertSongExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SongDao songDao;
        private InsertSongExecutorTask(SongDao songDao) {
            this.songDao = songDao;
        }
        protected void execute(Song song){
            service.execute(() -> songDao.insert(song));
        }
    }

    //update song executor task
    private static class UpdateSongExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SongDao songDao;
        private UpdateSongExecutorTask(SongDao songDao) {
            this.songDao = songDao;
        }
        protected void execute(Song song){
            service.execute(() -> songDao.update(song));
        }
    }

    //delete song executor task
    private static class DeleteSongExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SongDao songDao;
        private DeleteSongExecutorTask(SongDao songDao) {
            this.songDao = songDao;
        }
        protected void execute(Song song){
            service.execute(() -> songDao.delete(song));
        }
    }

    //find song executor task
    private static class FindSongExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SongDao songDao;
        private FindSongExecutorTask(SongDao songDao) {
            this.songDao = songDao;
        }
        protected Song find(int userID, String songName) {
            try {
                return service.submit(() -> songDao.findSong(userID, songName)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
