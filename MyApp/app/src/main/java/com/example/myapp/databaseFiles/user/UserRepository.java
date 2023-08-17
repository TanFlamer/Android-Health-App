package com.example.myapp.databaseFiles.user;

import android.app.Application;

import com.example.myapp.Database;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class UserRepository {

    //user data access object
    private final UserDao userDao;

    //constructor for user repository
    public UserRepository(Application application) {
        Database database = Database.getInstance(application);
        userDao = database.getUserDao();
    }

    //insert operation for user repository
    public long insert(User user) {
        return new InsertUserExecutorTask(userDao).execute(user);
    }

    //update operation for user repository
    public void update(User user) {
        new UpdateUserExecutorTask(userDao).execute(user);
    }

    //delete operation for user repository
    public void delete(User user) {
        new DeleteUserExecutorTask(userDao).execute(user);
    }

    //check if user with specific name exists
    public User findUser(String username) {
        return new FindUserExecutorTask(userDao).find(username);
    }

    //insert user executor task
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

    //update user executor task
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

    //delete user executor task
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

    //find user executor task
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
    }
}
