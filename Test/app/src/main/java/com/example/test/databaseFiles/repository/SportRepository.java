package com.example.test.databaseFiles.repository;

import android.app.Application;

import com.example.test.databaseFiles.Database;
import com.example.test.databaseFiles.dao.SportDao;
import com.example.test.databaseFiles.entity.Sport;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SportRepository {

    private SportDao sportDao;
    private List<Sport> allSport;

    public SportRepository(Application application) {
        Database database = Database.getInstance(application);
        sportDao = database.getSportDao();
        allSport = sportDao.getAllSport();
    }

    public void insert(Sport sport) {
        new InsertSportExecutorTask(sportDao).execute(sport);
    }

    public void update(Sport sport) {
        new UpdateSportExecutorTask(sportDao).execute(sport);
    }

    public void delete(Sport sport) {
        new DeleteSportExecutorTask(sportDao).execute(sport);
    }

    public List<Sport> findSport(int sportID) {
        return new FindSportExecutorTask(sportDao).get(sportID);
    }

    public List<Sport> getAllSport() {
        return allSport;
    }

    private static class InsertSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SportDao sportDao;
        private InsertSportExecutorTask(SportDao sportDao) {
            this.sportDao = sportDao;
        }
        protected void execute(Sport sport){
            service.execute(() -> sportDao.insert(sport));
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
        protected List<Sport> get(int sportID) {
            try {
                return service.submit(() -> sportDao.findSport(sportID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
