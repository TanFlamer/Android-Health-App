package com.example.myapp.databaseFiles.repository;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.databaseFiles.Database;
import com.example.myapp.databaseFiles.dao.SleepDao;
import com.example.myapp.databaseFiles.entity.Sleep;

import java.time.LocalDate;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SleepRepository {

    private SleepDao sleepDao;

    public SleepRepository(Application application) {
        Database database = Database.getInstance(application);
        sleepDao = database.getSleepDao();
    }

    public void insert(Sleep sleep) {
        new InsertSleepExecutorTask(sleepDao).execute(sleep);
    }

    public void update(Sleep sleep) {
        new UpdateSleepExecutorTask(sleepDao).execute(sleep);
    }

    public void delete(Sleep sleep) {
        new DeleteSleepExecutorTask(sleepDao).execute(sleep);
    }

    public List<Sleep> findSleep(int userID, LocalDate date) {
        return new FindSleepExecutorTask(sleepDao).get(userID, date);
    }

    public LiveData<List<Sleep>> getAllSleep(int userID) {
        return sleepDao.getAllSleep(userID);
    }

    private static class InsertSleepExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SleepDao sleepDao;
        private InsertSleepExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected void execute(Sleep sleep){
            service.execute(() -> sleepDao.insert(sleep));
        }
    }

    private static class UpdateSleepExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SleepDao sleepDao;
        private UpdateSleepExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected void execute(Sleep sleep){
            service.execute(() -> sleepDao.update(sleep));
        }
    }

    private static class DeleteSleepExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SleepDao sleepDao;
        private DeleteSleepExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected void execute(Sleep sleep){
            service.execute(() -> sleepDao.delete(sleep));
        }
    }

    private static class FindSleepExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SleepDao sleepDao;
        private FindSleepExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected List<Sleep> get(int userID, LocalDate date) {
            try {
                return service.submit(() -> sleepDao.findSleep(userID, date)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
