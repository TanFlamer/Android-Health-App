package com.example.myapp.databaseFiles.repository;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.databaseFiles.Database;
import com.example.myapp.databaseFiles.dao.SportDao;
import com.example.myapp.databaseFiles.entity.Sport;
import com.example.myapp.databaseFiles.entity.User;

import java.time.LocalDate;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SportRepository {

    private SportDao sportDao;

    public SportRepository(Application application) {
        Database database = Database.getInstance(application);
        sportDao = database.getSportDao();
    }

    public long insert(Sport sport) {
        return new InsertSportExecutorTask(sportDao).execute(sport);
    }

    public void update(Sport sport) {
        new UpdateSportExecutorTask(sportDao).execute(sport);
    }

    public void delete(Sport sport) {
        new DeleteSportExecutorTask(sportDao).execute(sport);
    }

    public List<Sport> findSport(int userID, LocalDate date) {
        return new FindSportExecutorTask(sportDao).find(userID, date);
    }

    public List<Sport> getSport(int sportID) {
        return new FindSportExecutorTask(sportDao).get(sportID);
    }

    public LiveData<List<Sport>> getAllSport(int userID) {
        return sportDao.getAllSport(userID);
    }

    private static class InsertSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SportDao sportDao;
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

    private static class UpdateSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SportDao sportDao;
        private UpdateSportExecutorTask(SportDao sportDao) {
            this.sportDao = sportDao;
        }
        protected void execute(Sport sport){
            service.execute(() -> sportDao.update(sport));
        }
    }

    private static class DeleteSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SportDao sportDao;
        private DeleteSportExecutorTask(SportDao sportDao) {
            this.sportDao = sportDao;
        }
        protected void execute(Sport sport){
            service.execute(() -> sportDao.delete(sport));
        }
    }

    private static class FindSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SportDao sportDao;
        private FindSportExecutorTask(SportDao sportDao) {
            this.sportDao = sportDao;
        }
        protected List<Sport> find(int userID, LocalDate date) {
            try {
                return service.submit(() -> sportDao.findSport(userID, date)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
        protected List<Sport> get(int sportID) {
            try {
                return service.submit(() -> sportDao.getSport(sportID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
