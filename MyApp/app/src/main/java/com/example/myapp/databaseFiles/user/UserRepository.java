package com.example.myapp.databaseFiles.user;

import android.app.Application;

import com.example.myapp.Database;
import com.example.myapp.databaseFiles.user.UserDao;
import com.example.myapp.databaseFiles.user.User;

import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class UserRepository {

    private UserDao userDao;

    public UserRepository(Application application) {
        Database database = Database.getInstance(application);
        userDao = database.getUserDao();
    }

    public long insert(User user) {
        return new InsertUserExecutorTask(userDao).execute(user);
    }

    public void update(User user) {
        new UpdateUserExecutorTask(userDao).execute(user);
    }

    public void delete(User user) {
        new DeleteUserExecutorTask(userDao).execute(user);
    }

    public List<User> findUser(String username) {
        return new FindUserExecutorTask(userDao).get(username);
    }

    private static class InsertUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private UserDao userDao;
        private InsertUserExecutorTask(UserDao userDao) {
            this.userDao = userDao;
        }
        protected long execute(User user) {
            try{
                return (long) service.submit((Callable<Object>) () -> userDao.insert(user)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return 0;
        }
    }

    private static class UpdateUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private UserDao userDao;
        private UpdateUserExecutorTask(UserDao userDao) {
            this.userDao = userDao;
        }
        protected void execute(User user){
            service.execute(() -> userDao.update(user));
        }
    }

    private static class DeleteUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private UserDao userDao;
        private DeleteUserExecutorTask(UserDao userDao) {
            this.userDao = userDao;
        }
        protected void execute(User user){
            service.execute(() -> userDao.delete(user));
        }
    }

    private static class FindUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private UserDao userDao;
        private FindUserExecutorTask(UserDao userDao) {
            this.userDao = userDao;
        }
        protected List<User> get(String username) {
            try {
                return service.submit(() -> userDao.findUser(username)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
