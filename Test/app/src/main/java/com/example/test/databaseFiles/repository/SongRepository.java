package com.example.test.databaseFiles.repository;

import android.app.Application;

import com.example.test.databaseFiles.Database;
import com.example.test.databaseFiles.dao.SongDao;
import com.example.test.databaseFiles.entity.Song;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SongRepository {

    private SongDao songDao;
    private List<Song> allSongs;

    public SongRepository(Application application) {
        Database database = Database.getInstance(application);
        songDao = database.getSongDao();
        allSongs = songDao.getAllSongs();
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

    public List<Song> findSong(int songID) {
        return new FindSongExecutorTask(songDao).get(songID);
    }

    public List<Song> getAllSongs() {
        return allSongs;
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
        protected List<Song> get(int songID) {
            try {
                return service.submit(() -> songDao.findSong(songID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
