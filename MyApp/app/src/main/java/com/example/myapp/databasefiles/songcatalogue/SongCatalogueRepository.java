package com.example.myapp.databaseFiles.songcatalogue;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.Database;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SongCatalogueRepository {

    //song catalogue data access object
    private final SongCatalogueDao songCatalogueDao;

    //constructor for song catalogue repository
    public SongCatalogueRepository(Application application) {
        Database database = Database.getInstance(application);
        songCatalogueDao = database.getSongPlaylistDao();
    }

    //insert operation for song catalogue repository
    public void insert(SongCatalogue songCatalogue) {
        new InsertSongCatalogueExecutorTask(songCatalogueDao).execute(songCatalogue);
    }

    //update operation for song catalogue repository
    public void update(SongCatalogue songCatalogue) {
        new UpdateSongCatalogueExecutorTask(songCatalogueDao).execute(songCatalogue);
    }

    //delete operation for song catalogue repository
    public void delete(SongCatalogue songCatalogue) {
        new DeleteSongCatalogueExecutorTask(songCatalogueDao).execute(songCatalogue);
    }

    //returns live data of all song catalogues belonging to a user
    public LiveData<List<SongCatalogue>> getAllSongCatalogue(int userID) {
        return songCatalogueDao.getAllSongCatalogue(userID);
    }

    //insert song catalogue executor task
    private static class InsertSongCatalogueExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SongCatalogueDao songCatalogueDao;
        private InsertSongCatalogueExecutorTask(SongCatalogueDao songCatalogueDao) {
            this.songCatalogueDao = songCatalogueDao;
        }
        protected void execute(SongCatalogue songCatalogue){
            service.execute(() -> songCatalogueDao.insert(songCatalogue));
        }
    }

    //update song catalogue executor task
    private static class UpdateSongCatalogueExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SongCatalogueDao songCatalogueDao;
        private UpdateSongCatalogueExecutorTask(SongCatalogueDao songCatalogueDao) {
            this.songCatalogueDao = songCatalogueDao;
        }
        protected void execute(SongCatalogue songCatalogue){
            service.execute(() -> songCatalogueDao.update(songCatalogue));
        }
    }

    //insert delete catalogue executor task
    private static class DeleteSongCatalogueExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SongCatalogueDao songCatalogueDao;
        private DeleteSongCatalogueExecutorTask(SongCatalogueDao songCatalogueDao) {
            this.songCatalogueDao = songCatalogueDao;
        }
        protected void execute(SongCatalogue songCatalogue){
            service.execute(() -> songCatalogueDao.delete(songCatalogue));
        }
    }
}
