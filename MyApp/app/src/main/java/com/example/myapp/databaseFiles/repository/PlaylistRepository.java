package com.example.myapp.databaseFiles.repository;

import android.app.Application;

import com.example.myapp.databaseFiles.Database;
import com.example.myapp.databaseFiles.dao.PlaylistDao;
import com.example.myapp.databaseFiles.entity.Playlist;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class PlaylistRepository {

    private PlaylistDao playlistDao;
    private List<Playlist> allPlaylists;

    public PlaylistRepository(Application application) {
        Database database = Database.getInstance(application);
        playlistDao = database.getPlaylistDao();
        allPlaylists = playlistDao.getAllPlaylists();
    }

    public void insert(Playlist playlist) {
        new InsertPlaylistExecutorTask(playlistDao).execute(playlist);
    }

    public void update(Playlist playlist) {
        new UpdatePlaylistExecutorTask(playlistDao).execute(playlist);
    }

    public void delete(Playlist playlist) {
        new DeletePlaylistExecutorTask(playlistDao).execute(playlist);
    }

    public Playlist findPlaylist(int playlistID) {
        return new FindPlaylistExecutorTask(playlistDao).get(playlistID);
    }

    public List<Playlist> getAllPlaylists() {
        return allPlaylists;
    }

    private static class InsertPlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private PlaylistDao playlistDao;
        private InsertPlaylistExecutorTask(PlaylistDao playlistDao) {
            this.playlistDao = playlistDao;
        }
        protected void execute(Playlist playlist){
            service.execute(() -> playlistDao.insert(playlist));
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
        protected Playlist get(int playlistID) {
            try {
                return service.submit(() -> playlistDao.findPlaylist(playlistID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
