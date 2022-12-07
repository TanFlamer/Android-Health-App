package com.example.myapp.databasefiles.playlist;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.Database;

import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class PlaylistRepository {

    //playlist data access object
    private final PlaylistDao playlistDao;

    //constructor for playlist repository
    public PlaylistRepository(Application application) {
        Database database = Database.getInstance(application);
        playlistDao = database.getPlaylistDao();
    }

    //insert operation for playlist repository
    public long insert(Playlist playlist) {
        return new InsertPlaylistExecutorTask(playlistDao).execute(playlist);
    }

    //update operation for playlist repository
    public void update(Playlist playlist) {
        new UpdatePlaylistExecutorTask(playlistDao).execute(playlist);
    }

    //delete operation for playlist repository
    public void delete(Playlist playlist) {
        new DeletePlaylistExecutorTask(playlistDao).execute(playlist);
    }

    //check if playlist with specific name exists for a user
    public Playlist findPlaylist(int userID, String playlistName) {
        return new FindPlaylistExecutorTask(playlistDao).find(userID, playlistName);
    }

    //returns live data of all playlist belonging to a user
    public LiveData<List<Playlist>> getAllPlaylists(int userID) {
        return playlistDao.getAllPlaylists(userID);
    }

    //insert playlist executor task
    private static class InsertPlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final PlaylistDao playlistDao;
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

    //update playlist executor task
    private static class UpdatePlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final PlaylistDao playlistDao;
        private UpdatePlaylistExecutorTask(PlaylistDao playlistDao) {
            this.playlistDao = playlistDao;
        }
        protected void execute(Playlist playlist){
            service.execute(() -> playlistDao.update(playlist));
        }
    }

    //delete playlist executor task
    private static class DeletePlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final PlaylistDao playlistDao;
        private DeletePlaylistExecutorTask(PlaylistDao playlistDao) {
            this.playlistDao = playlistDao;
        }
        protected void execute(Playlist playlist){
            service.execute(() -> playlistDao.delete(playlist));
        }
    }

    //find playlist executor task
    private static class FindPlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final PlaylistDao playlistDao;
        private FindPlaylistExecutorTask(PlaylistDao playlistDao) {
            this.playlistDao = playlistDao;
        }
        protected Playlist find(int userID, String playlistName) {
            try {
                return service.submit(() -> playlistDao.findPlaylist(userID, playlistName)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
