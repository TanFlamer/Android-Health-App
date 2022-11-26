package com.example.myapp.databaseFiles.song;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.Database;
import com.example.myapp.databaseFiles.song.SongDao;
import com.example.myapp.databaseFiles.song.Song;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SongRepository {

    private SongDao songDao;

    public SongRepository(Application application) {
        Database database = Database.getInstance(application);
        songDao = database.getSongDao();
    }

    public void insert(Song song) {
        new InsertSongExecutorTask(songDao).execute(song);
    }

    public void update(Song song) {
        new UpdateSongExecutorTask(songDao).execute(song);
    }

    public void delete(Song song) {
        new DeleteSongExecutorTask(songDao).execute(song);
    }

    public List<Song> getSong(int songID) {
        return new FindSongExecutorTask(songDao).get(songID);
    }

    public List<Song> findSong(int userID, String songName) {
        return new FindSongExecutorTask(songDao).find(userID, songName);
    }

    public LiveData<List<Song>> getAllSongs(int userID) {
        return songDao.getAllSongs(userID);
    }

    private static class InsertSongExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SongDao songDao;
        private InsertSongExecutorTask(SongDao songDao) {
            this.songDao = songDao;
        }
        protected void execute(Song song){
            service.execute(() -> songDao.insert(song));
        }
    }

    private static class UpdateSongExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SongDao songDao;
        private UpdateSongExecutorTask(SongDao songDao) {
            this.songDao = songDao;
        }
        protected void execute(Song song){
            service.execute(() -> songDao.update(song));
        }
    }

    private static class DeleteSongExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SongDao songDao;
        private DeleteSongExecutorTask(SongDao songDao) {
            this.songDao = songDao;
        }
        protected void execute(Song song){
            service.execute(() -> songDao.delete(song));
        }
    }

    private static class FindSongExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SongDao songDao;
        private FindSongExecutorTask(SongDao songDao) {
            this.songDao = songDao;
        }
        protected List<Song> find(int userID, String songName) {
            try {
                return service.submit(() -> songDao.findSong(userID, songName)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
        protected List<Song> get(int songID) {
            try {
                return service.submit(() -> songDao.getSong(songID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
