package com.example.myapp.databasefiles.songcatalogue;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.Database;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SongCatalogueRepository {

    private final SongCatalogueDao songCatalogueDao;

    public SongCatalogueRepository(Application application) {
        Database database = Database.getInstance(application);
        songCatalogueDao = database.getSongPlaylistDao();
    }

    public void insert(SongCatalogue songCatalogue) {
        new InsertSongPlaylistExecutorTask(songCatalogueDao).execute(songCatalogue);
    }

    public void update(SongCatalogue songCatalogue) {
        new UpdateSongPlaylistExecutorTask(songCatalogueDao).execute(songCatalogue);
    }

    public void delete(SongCatalogue songCatalogue) {
        new DeleteSongPlaylistExecutorTask(songCatalogueDao).execute(songCatalogue);
    }

    public LiveData<List<SongCatalogue>> getAllSongCatalogue(int userID) {
        return songCatalogueDao.getAllSongCatalogue(userID);
    }

    private static class InsertSongPlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SongCatalogueDao songCatalogueDao;
        private InsertSongPlaylistExecutorTask(SongCatalogueDao songCatalogueDao) {
            this.songCatalogueDao = songCatalogueDao;
        }
        protected void execute(SongCatalogue songCatalogue){
            service.execute(() -> songCatalogueDao.insert(songCatalogue));
        }
    }

    private static class UpdateSongPlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SongCatalogueDao songCatalogueDao;
        private UpdateSongPlaylistExecutorTask(SongCatalogueDao songCatalogueDao) {
            this.songCatalogueDao = songCatalogueDao;
        }
        protected void execute(SongCatalogue songCatalogue){
            service.execute(() -> songCatalogueDao.update(songCatalogue));
        }
    }

    private static class DeleteSongPlaylistExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SongCatalogueDao songCatalogueDao;
        private DeleteSongPlaylistExecutorTask(SongCatalogueDao songCatalogueDao) {
            this.songCatalogueDao = songCatalogueDao;
        }
        protected void execute(SongCatalogue songCatalogue){
            service.execute(() -> songCatalogueDao.delete(songCatalogue));
        }
    }
}
