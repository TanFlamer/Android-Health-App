package com.example.myapp.databaseFiles.repository;

import android.app.Application;

import com.example.myapp.databaseFiles.Database;
import com.example.myapp.databaseFiles.dao.SongDao;
import com.example.myapp.databaseFiles.entity.Song;

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
        new InsertUserExecutorTask(songDao).execute(song);
    }

    public void update(Song song) {
        new UpdateUserExecutorTask(songDao).execute(song);
    }

    public void delete(Song song) {
        new DeleteUserExecutorTask(songDao).execute(song);
    }

    public Song findSong(int songID) {
        return new FindUserExecutorTask(songDao).get(songID);
    }

    public List<Song> getAllSongs() {
        return allSongs;
    }

    private static class InsertUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SongDao songDao;
        private InsertUserExecutorTask(SongDao songDao) {
            this.songDao = songDao;
        }
        protected void execute(Song song){
            service.execute(() -> songDao.insert(song));
        }
    }

    private static class UpdateUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SongDao songDao;
        private UpdateUserExecutorTask(SongDao songDao) {
            this.songDao = songDao;
        }
        protected void execute(Song song){
            service.execute(() -> songDao.update(song));
        }
    }

    private static class DeleteUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SongDao songDao;
        private DeleteUserExecutorTask(SongDao songDao) {
            this.songDao = songDao;
        }
        protected void execute(Song song){
            service.execute(() -> songDao.delete(song));
        }
    }

    private static class FindUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SongDao songDao;
        private FindUserExecutorTask(SongDao songDao) {
            this.songDao = songDao;
        }
        protected Song get(int songID) {
            try {
                return service.submit(() -> songDao.findSong(songID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
