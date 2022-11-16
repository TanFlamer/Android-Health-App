package com.example.myapp.databaseFiles.repository;

import android.app.Application;

import com.example.myapp.databaseFiles.Database;
import com.example.myapp.databaseFiles.dao.TypeDao;
import com.example.myapp.databaseFiles.entity.Type;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class TypeRepository {

    private TypeDao typeDao;
    private List<Type> allTypes;

    public TypeRepository(Application application) {
        Database database = Database.getInstance(application);
        typeDao = database.getTypeDao();
        allTypes = typeDao.getAllTypes();
    }

    public void insert(Type type) {
        new InsertUserExecutorTask(typeDao).execute(type);
    }

    public void update(Type type) {
        new UpdateUserExecutorTask(typeDao).execute(type);
    }

    public void delete(Type type) {
        new DeleteUserExecutorTask(typeDao).execute(type);
    }

    public Type findType(int typeID) {
        return new FindUserExecutorTask(typeDao).get(typeID);
    }

    public List<Type> getAllTypes() {
        return allTypes;
    }

    private static class InsertUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private TypeDao typeDao;
        private InsertUserExecutorTask(TypeDao typeDao) {
            this.typeDao = typeDao;
        }
        protected void execute(Type type){
            service.execute(() -> typeDao.insert(type));
        }
    }

    private static class UpdateUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private TypeDao typeDao;
        private UpdateUserExecutorTask(TypeDao typeDao) {
            this.typeDao = typeDao;
        }
        protected void execute(Type type){
            service.execute(() -> typeDao.update(type));
        }
    }

    private static class DeleteUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private TypeDao typeDao;
        private DeleteUserExecutorTask(TypeDao typeDao) {
            this.typeDao = typeDao;
        }
        protected void execute(Type type){
            service.execute(() -> typeDao.delete(type));
        }
    }

    private static class FindUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private TypeDao typeDao;
        private FindUserExecutorTask(TypeDao typeDao) {
            this.typeDao = typeDao;
        }
        protected Type get(int typeID) {
            try {
                return service.submit(() -> typeDao.findType(typeID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
