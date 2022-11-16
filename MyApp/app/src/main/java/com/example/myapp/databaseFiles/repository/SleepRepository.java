package com.example.myapp.databaseFiles.repository;

import android.app.Application;

import com.example.myapp.databaseFiles.Database;
import com.example.myapp.databaseFiles.dao.SleepDao;
import com.example.myapp.databaseFiles.entity.Sleep;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SleepRepository {

    private SleepDao sleepDao;
    private List<Sleep> allSleep;

    public SleepRepository(Application application) {
        Database database = Database.getInstance(application);
        sleepDao = database.getSleepDao();
        allSleep = sleepDao.getAllSleep();
    }

    public void insert(Sleep sleep) {
        new InsertUserExecutorTask(sleepDao).execute(sleep);
    }

    public void update(Sleep sleep) {
        new UpdateUserExecutorTask(sleepDao).execute(sleep);
    }

    public void delete(Sleep sleep) {
        new DeleteUserExecutorTask(sleepDao).execute(sleep);
    }

    public Sleep findSleep(int sleepID) {
        return new FindUserExecutorTask(sleepDao).get(sleepID);
    }

    public List<Sleep> getAllSleep() {
        return allSleep;
    }

    private static class InsertUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SleepDao sleepDao;
        private InsertUserExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected void execute(Sleep sleep){
            service.execute(() -> sleepDao.insert(sleep));
        }
    }

    private static class UpdateUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SleepDao sleepDao;
        private UpdateUserExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected void execute(Sleep sleep){
            service.execute(() -> sleepDao.update(sleep));
        }
    }

    private static class DeleteUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SleepDao sleepDao;
        private DeleteUserExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected void execute(Sleep sleep){
            service.execute(() -> sleepDao.delete(sleep));
        }
    }

    private static class FindUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SleepDao sleepDao;
        private FindUserExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected Sleep get(int sleepID) {
            try {
                return service.submit(() -> sleepDao.findSleep(sleepID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
