package com.example.myapp.databaseFiles.playlist;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.Database;
import com.example.myapp.databaseFiles.playlist.PlaylistDao;
import com.example.myapp.databaseFiles.playlist.Playlist;

import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class PlaylistRepository {

    private PlaylistDao playlistDao;

    public PlaylistRepository(Application application) {
        Database database = Database.getInstance(application);
        playlistDao = database.getPlaylistDao();
    }

    public long insert(Playlist playlist) {
        return new InsertPlaylistExecutorTask(playlistDao).execute(playlist);
    }

    public void update(Playlist playlist) {
        new UpdatePlaylistExecutorTask(playlistDao).execute(playlist);
    }

    public void delete(Playlist playlist) {
        new DeletePlaylistExecutorTask(playlistDao).execute(playlist);
    }

    public List<Playlist> getPlaylist(int playlistID) {
        return new FindPlaylistExecutorTask(playlistDao).get(playlistID);
    }

    public List<Playlist> findPlaylist(int userID, String playlistName) {
        return new FindPlaylistExecutorTask(playlistDao).find(userID, playlistName);
    }

    public LiveData<List<Playlist>> getAllPlaylists(int userID) {
        return playlistDao.getAllPlaylists(userID);
    }

    private static class InsertPlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private PlaylistDao playlistDao;
        private InsertPlaylistExecutorTask(PlaylistDao playlistDao) {
            this.playlistDao = playlistDao;
        }
        protected long execute(Playlist playlist) {
            try{
                return (long) service.submit((Callable<Object>) () -> playlistDao.insert(playlist)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return 0;
        }
    }

    private static class UpdatePlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private PlaylistDao playlistDao;
        private UpdatePlaylistExecutorTask(PlaylistDao playlistDao) {
            this.playlistDao = playlistDao;
        }
        protected void execute(Playlist playlist){
            service.execute(() -> playlistDao.update(playlist));
        }
    }

    private static class DeletePlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private PlaylistDao playlistDao;
        private DeletePlaylistExecutorTask(PlaylistDao playlistDao) {
            this.playlistDao = playlistDao;
        }
        protected void execute(Playlist playlist){
            service.execute(() -> playlistDao.delete(playlist));
        }
    }

    private static class FindPlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private PlaylistDao playlistDao;
        private FindPlaylistExecutorTask(PlaylistDao playlistDao) {
            this.playlistDao = playlistDao;
        }
        protected List<Playlist> get(int playlistID) {
            try {
                return service.submit(() -> playlistDao.getPlaylist(playlistID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
        protected List<Playlist> find(int userID, String playlistName) {
            try {
                return service.submit(() -> playlistDao.findPlaylist(userID, playlistName)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
