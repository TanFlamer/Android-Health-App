package com.example.myapp.databasefiles.user;

import android.app.Application;

import com.example.myapp.Database;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class UserRepository {

    private final UserDao userDao;

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

    public User findUser(String username) {
        return new FindUserExecutorTask(userDao).find(username);
    }

    public User getUser(int userID) {
        return new FindUserExecutorTask(userDao).get(userID);
    }

    private static class InsertUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final UserDao userDao;
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
        private final UserDao userDao;
        private UpdateUserExecutorTask(UserDao userDao) {
            this.userDao = userDao;
        }
        protected void execute(User user){
            service.execute(() -> userDao.update(user));
        }
    }

    private static class DeleteUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final UserDao userDao;
        private DeleteUserExecutorTask(UserDao userDao) {
            this.userDao = userDao;
        }
        protected void execute(User user){
            service.execute(() -> userDao.delete(user));
        }
    }

    private static class FindUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final UserDao userDao;
        private FindUserExecutorTask(UserDao userDao) {
            this.userDao = userDao;
        }
        protected User find(String username) {
            try {
                return service.submit(() -> userDao.findUser(username)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
        protected User get(int userID) {
            try {
                return service.submit(() -> userDao.getUser(userID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
