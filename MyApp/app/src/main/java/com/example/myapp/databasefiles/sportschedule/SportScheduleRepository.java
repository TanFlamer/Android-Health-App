package com.example.myapp.databasefiles.sportschedule;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.Database;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SportScheduleRepository {

    private final SportScheduleDao sportScheduleDao;

    public SportScheduleRepository(Application application) {
        Database database = Database.getInstance(application);
        sportScheduleDao = database.getTypeSportDao();
    }

    public void insert(SportSchedule sportSchedule) {
        new InsertTypeSportExecutorTask(sportScheduleDao).execute(sportSchedule);
    }

    public void update(SportSchedule sportSchedule) {
        new UpdateTypeSportExecutorTask(sportScheduleDao).execute(sportSchedule);
    }

    public void delete(SportSchedule sportSchedule) {
        new DeleteTypeSportExecutorTask(sportScheduleDao).execute(sportSchedule);
    }

    public LiveData<List<SportSchedule>> getAllTypeSport(int userID) {
        return sportScheduleDao.getAllTypeSport(userID);
    }

    private static class InsertTypeSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SportScheduleDao sportScheduleDao;
        private InsertTypeSportExecutorTask(SportScheduleDao sportScheduleDao) {
            this.sportScheduleDao = sportScheduleDao;
        }
        protected void execute(SportSchedule sportSchedule){
            service.execute(() -> sportScheduleDao.insert(sportSchedule));
        }
    }

    private static class UpdateTypeSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SportScheduleDao sportScheduleDao;
        private UpdateTypeSportExecutorTask(SportScheduleDao sportScheduleDao) {
            this.sportScheduleDao = sportScheduleDao;
        }
        protected void execute(SportSchedule sportSchedule){
            service.execute(() -> sportScheduleDao.update(sportSchedule));
        }
    }

    private static class DeleteTypeSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SportScheduleDao sportScheduleDao;
        private DeleteTypeSportExecutorTask(SportScheduleDao sportScheduleDao) {
            this.sportScheduleDao = sportScheduleDao;
        }
        protected void execute(SportSchedule sportSchedule){
            service.execute(() -> sportScheduleDao.delete(sportSchedule));
        }
    }
}
