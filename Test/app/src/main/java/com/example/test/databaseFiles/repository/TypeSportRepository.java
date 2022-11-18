package com.example.test.databaseFiles.repository;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.test.databaseFiles.Database;
import com.example.test.databaseFiles.dao.TypeSportDao;
import com.example.test.databaseFiles.entity.TypeSport;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class TypeSportRepository {

    private TypeSportDao typeSportDao;

    public TypeSportRepository(Application application) {
        Database database = Database.getInstance(application);
        typeSportDao = database.getTypeSportDao();
    }

    public void insert(TypeSport typeSport) {
        new InsertTypeSportExecutorTask(typeSportDao).execute(typeSport);
    }

    public void update(TypeSport typeSport) {
        new UpdateTypeSportExecutorTask(typeSportDao).execute(typeSport);
    }

    public void delete(TypeSport typeSport) {
        new DeleteTypeSportExecutorTask(typeSportDao).execute(typeSport);
    }

    public List<TypeSport> findTypeSport(int sportID, int typeID) {
        return new FindTypeSportExecutorTask(typeSportDao).get(sportID, typeID);
    }

    public LiveData<List<TypeSport>> getAllTypeSport(int sportID) {
        return typeSportDao.getAllTypeSport(sportID);
    }

    private static class InsertTypeSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private TypeSportDao typeSportDao;
        private InsertTypeSportExecutorTask(TypeSportDao typeSportDao) {
            this.typeSportDao = typeSportDao;
        }
        protected void execute(TypeSport typeSport){
            service.execute(() -> typeSportDao.insert(typeSport));
        }
    }

    private static class UpdateTypeSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private TypeSportDao typeSportDao;
        private UpdateTypeSportExecutorTask(TypeSportDao typeSportDao) {
            this.typeSportDao = typeSportDao;
        }
        protected void execute(TypeSport typeSport){
            service.execute(() -> typeSportDao.update(typeSport));
        }
    }

    private static class DeleteTypeSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private TypeSportDao typeSportDao;
        private DeleteTypeSportExecutorTask(TypeSportDao typeSportDao) {
            this.typeSportDao = typeSportDao;
        }
        protected void execute(TypeSport typeSport){
            service.execute(() -> typeSportDao.delete(typeSport));
        }
    }

    private static class FindTypeSportExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private TypeSportDao typeSportDao;
        private FindTypeSportExecutorTask(TypeSportDao typeSportDao) {
            this.typeSportDao = typeSportDao;
        }
        protected List<TypeSport> get(int sportID, int typeID) {
            try {
                return service.submit(() -> typeSportDao.findTypeSport(sportID, typeID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}