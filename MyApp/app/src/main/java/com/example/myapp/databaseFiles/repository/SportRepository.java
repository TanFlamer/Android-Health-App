package com.example.myapp.databaseFiles.repository;

import android.app.Application;

import com.example.myapp.databaseFiles.Database;
import com.example.myapp.databaseFiles.dao.SportDao;
import com.example.myapp.databaseFiles.entity.Sport;

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
        new InsertUserExecutorTask(sportDao).execute(sport);
    }

    public void update(Sport sport) {
        new UpdateUserExecutorTask(sportDao).execute(sport);
    }

    public void delete(Sport sport) {
        new DeleteUserExecutorTask(sportDao).execute(sport);
    }

    public Sport findSport(int sportID) {
        return new FindUserExecutorTask(sportDao).get(sportID);
    }

    public List<Sport> getAllSport() {
        return allSport;
    }

    private static class InsertUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SportDao sportDao;
        private InsertUserExecutorTask(SportDao sportDao) {
            this.sportDao = sportDao;
        }
        protected void execute(Sport sport){
            service.execute(() -> sportDao.insert(sport));
        }
    }

    private static class UpdateUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SportDao sportDao;
        private UpdateUserExecutorTask(SportDao sportDao) {
            this.sportDao = sportDao;
        }
        protected void execute(Sport sport){
            service.execute(() -> sportDao.update(sport));
        }
    }

    private static class DeleteUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SportDao sportDao;
        private DeleteUserExecutorTask(SportDao sportDao) {
            this.sportDao = sportDao;
        }
        protected void execute(Sport sport){
            service.execute(() -> sportDao.delete(sport));
        }
    }

    private static class FindUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SportDao sportDao;
        private FindUserExecutorTask(SportDao sportDao) {
            this.sportDao = sportDao;
        }
        protected Sport get(int sportID) {
            try {
                return service.submit(() -> sportDao.findSport(sportID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
