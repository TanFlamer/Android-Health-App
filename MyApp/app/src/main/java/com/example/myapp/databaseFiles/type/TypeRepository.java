package com.example.myapp.databasefiles.type;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.Database;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class TypeRepository {

    //sport type data access object
    private final TypeDao typeDao;

    //constructor for sport type repository
    public TypeRepository(Application application) {
        Database database = Database.getInstance(application);
        typeDao = database.getTypeDao();
    }

    //insert operation for sport type repository
    public void insert(Type type) {
        new InsertTypeExecutorTask(typeDao).execute(type);
    }

    //update operation for sport type repository
    public void update(Type type) {
        new UpdateTypeExecutorTask(typeDao).execute(type);
    }

    //delete operation for sport type repository
    public void delete(Type type) {
        new DeleteTypeExecutorTask(typeDao).execute(type);
    }

    //check if sport type with specific name exists for a user
    public Type findType(int userID, String typeName) {
        return new FindTypeExecutorTask(typeDao).find(userID, typeName);
    }

    //returns live data of all sport types belonging to a user
    public LiveData<List<Type>> getAllTypes(int userID) {
        return typeDao.getAllTypes(userID);
    }

    //insert sport type executor task
    private static class InsertTypeExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final TypeDao typeDao;
        private InsertTypeExecutorTask(TypeDao typeDao) {
            this.typeDao = typeDao;
        }
        protected void execute(Type type){
            service.execute(() -> typeDao.insert(type));
        }
    }

    //update sport type executor task
    private static class UpdateTypeExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final TypeDao typeDao;
        private UpdateTypeExecutorTask(TypeDao typeDao) {
            this.typeDao = typeDao;
        }
        protected void execute(Type type){
            service.execute(() -> typeDao.update(type));
        }
    }

    //delete sport type executor task
    private static class DeleteTypeExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final TypeDao typeDao;
        private DeleteTypeExecutorTask(TypeDao typeDao) {
            this.typeDao = typeDao;
        }
        protected void execute(Type type){
            service.execute(() -> typeDao.delete(type));
        }
    }

    //find sport type executor task
    private static class FindTypeExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final TypeDao typeDao;
        private FindTypeExecutorTask(TypeDao typeDao) {
            this.typeDao = typeDao;
        }
        protected Type find(int userID, String typeName) {
            try {
                return service.submit(() -> typeDao.findType(userID, typeName)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
