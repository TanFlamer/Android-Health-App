package com.example.myapp.databaseFiles.repository;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.databaseFiles.Database;
import com.example.myapp.databaseFiles.dao.TypeDao;
import com.example.myapp.databaseFiles.entity.Type;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class TypeRepository {

    private TypeDao typeDao;

    public TypeRepository(Application application) {
        Database database = Database.getInstance(application);
        typeDao = database.getTypeDao();
    }

    public void insert(Type type) {
        new InsertTypeExecutorTask(typeDao).execute(type);
    }

    public void update(Type type) {
        new UpdateTypeExecutorTask(typeDao).execute(type);
    }

    public void delete(Type type) {
        new DeleteTypeExecutorTask(typeDao).execute(type);
    }

    public List<Type> findType(int typeID) {
        return new FindTypeExecutorTask(typeDao).get(typeID);
    }

    public LiveData<List<Type>> getAllTypes(int userID) {
        return typeDao.getAllTypes(userID);
    }

    private static class InsertTypeExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private TypeDao typeDao;
        private InsertTypeExecutorTask(TypeDao typeDao) {
            this.typeDao = typeDao;
        }
        protected void execute(Type type){
            service.execute(() -> typeDao.insert(type));
        }
    }

    private static class UpdateTypeExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private TypeDao typeDao;
        private UpdateTypeExecutorTask(TypeDao typeDao) {
            this.typeDao = typeDao;
        }
        protected void execute(Type type){
            service.execute(() -> typeDao.update(type));
        }
    }

    private static class DeleteTypeExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private TypeDao typeDao;
        private DeleteTypeExecutorTask(TypeDao typeDao) {
            this.typeDao = typeDao;
        }
        protected void execute(Type type){
            service.execute(() -> typeDao.delete(type));
        }
    }

    private static class FindTypeExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private TypeDao typeDao;
        private FindTypeExecutorTask(TypeDao typeDao) {
            this.typeDao = typeDao;
        }
        protected List<Type> get(int typeID) {
            try {
                return service.submit(() -> typeDao.findType(typeID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}