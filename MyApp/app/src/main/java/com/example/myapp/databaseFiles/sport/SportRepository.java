package com.example.myapp.databaseFiles.sport;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.Database;

import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SportRepository {

    //sport data data access object
    private final SportDao sportDao;

    //constructor for sport data repository
    public SportRepository(Application application) {
        Database database = Database.getInstance(application);
        sportDao = database.getSportDao();
    }

    //insert operation for sport data repository
    public long insert(Sport sport) {
        return new InsertSportExecutorTask(sportDao).execute(sport);
    }

    //update operation for sport data repository
    public void update(Sport sport) {
        new UpdateSportExecutorTask(sportDao).execute(sport);
    }

    //delete operation for sport data repository
    public void delete(Sport sport) {
        new DeleteSportExecutorTask(sportDao).execute(sport);
    }

    //check if sport data with specific date exists for a user
    public Sport findSport(int userID, long date) {
        return new FindSportExecutorTask(sportDao).find(userID, date);
    }

    //returns live data of all sport data belonging to a user
    public LiveData<List<Sport>> getAllSport(int userID) {
        return sportDao.getAllSport(userID);
    }

    //insert sport data executor task
    private static class InsertSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SportDao sportDao;
        private InsertSportExecutorTask(SportDao sportDao) {
            this.sportDao = sportDao;
        }
        protected long execute(Sport sport) {
            try{
                return (long) service.submit((Callable<Object>) () -> sportDao.insert(sport)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return 0;
        }
    }

    //update sport data executor task
    private static class UpdateSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SportDao sportDao;
        private UpdateSportExecutorTask(SportDao sportDao) {
            this.sportDao = sportDao;
        }
        protected void execute(Sport sport){
            service.execute(() -> sportDao.update(sport));
        }
    }

    //delete sport data executor task
    private static class DeleteSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SportDao sportDao;
        private DeleteSportExecutorTask(SportDao sportDao) {
            this.sportDao = sportDao;
        }
        protected void execute(Sport sport){
            service.execute(() -> sportDao.delete(sport));
        }
    }

    //find sport data executor task
    private static class FindSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SportDao sportDao;
        private FindSportExecutorTask(SportDao sportDao) {
            this.sportDao = sportDao;
        }
        protected Sport find(int userID, long date) {
            try {
                return service.submit(() -> sportDao.findSport(userID, date)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
