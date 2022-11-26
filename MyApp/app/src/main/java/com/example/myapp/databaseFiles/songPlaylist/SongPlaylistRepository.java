package com.example.myapp.databaseFiles.songPlaylist;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.Database;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylistDao;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylist;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SongPlaylistRepository {

    private SongPlaylistDao songPlaylistDao;

    public SongPlaylistRepository(Application application) {
        Database database = Database.getInstance(application);
        songPlaylistDao = database.getSongPlaylistDao();
    }

    public void insert(SongPlaylist songPlaylist) {
        new InsertSongPlaylistExecutorTask(songPlaylistDao).execute(songPlaylist);
    }

    public void update(SongPlaylist songPlaylist) {
        new UpdateSongPlaylistExecutorTask(songPlaylistDao).execute(songPlaylist);
    }

    public void delete(SongPlaylist songPlaylist) {
        new DeleteSongPlaylistExecutorTask(songPlaylistDao).execute(songPlaylist);
    }

    public List<SongPlaylist> findSongPlaylist(int playlistID, int songID) {
        return new FindSongPlaylistExecutorTask(songPlaylistDao).find(playlistID, songID);
    }

    public List<SongPlaylist> getSongPlaylist(int playlistID) {
        return new FindSongPlaylistExecutorTask(songPlaylistDao).get(playlistID);
    }

    public LiveData<List<SongPlaylist>> getAllSongPlaylist(int userID) {
        return songPlaylistDao.getAllSongPlaylist(userID);
    }

    private static class InsertSongPlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SongPlaylistDao songPlaylistDao;
        private InsertSongPlaylistExecutorTask(SongPlaylistDao songPlaylistDao) {
            this.songPlaylistDao = songPlaylistDao;
        }
        protected void execute(SongPlaylist songPlaylist){
            service.execute(() -> songPlaylistDao.insert(songPlaylist));
        }
    }

    private static class UpdateSongPlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SongPlaylistDao songPlaylistDao;
        private UpdateSongPlaylistExecutorTask(SongPlaylistDao songPlaylistDao) {
            this.songPlaylistDao = songPlaylistDao;
        }
        protected void execute(SongPlaylist songPlaylist){
            service.execute(() -> songPlaylistDao.update(songPlaylist));
        }
    }

    private static class DeleteSongPlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SongPlaylistDao songPlaylistDao;
        private DeleteSongPlaylistExecutorTask(SongPlaylistDao songPlaylistDao) {
            this.songPlaylistDao = songPlaylistDao;
        }
        protected void execute(SongPlaylist songPlaylist){
            service.execute(() -> songPlaylistDao.delete(songPlaylist));
        }
    }

    private static class FindSongPlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SongPlaylistDao songPlaylistDao;
        private FindSongPlaylistExecutorTask(SongPlaylistDao songPlaylistDao) {
            this.songPlaylistDao = songPlaylistDao;
        }
        protected List<SongPlaylist> find(int playlistID, int songID) {
            try {
                return service.submit(() -> songPlaylistDao.findSongPlaylist(playlistID, songID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
        protected List<SongPlaylist> get(int playlistID) {
            try {
                return service.submit(() -> songPlaylistDao.getSongPlaylist(playlistID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
