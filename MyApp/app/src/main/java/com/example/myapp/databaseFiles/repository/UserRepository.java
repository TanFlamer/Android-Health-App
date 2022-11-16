package com.example.myapp.databaseFiles.repository;

import android.app.Application;

import com.example.myapp.databaseFiles.Database;
import com.example.myapp.databaseFiles.dao.UserDao;
import com.example.myapp.databaseFiles.entity.User;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class UserRepository {

    private UserDao userDao;
    private List<User> allUsers;

    public UserRepository(Application application) {
        Database database = Database.getInstance(application);
        userDao = database.getUserDao();
        allUsers = userDao.getAllUsers();
    }

    public void insert(User user) {
        new InsertUserExecutorTask(userDao).execute(user);
    }

    public void update(User user) {
        new UpdateUserExecutorTask(userDao).execute(user);
    }

    public void delete(User user) {
        new DeleteUserExecutorTask(userDao).execute(user);
    }

    public User findUser(int userID) {
        return new FindUserExecutorTask(userDao).get(userID);
    }

    public List<User> getAllUsers() {
        return allUsers;
    }

    private static class InsertUserExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private UserDao userDao;
        private InsertUserExecutorTask(UserDao userDao) {
            this.userDao = userDao;
        }
        protected void execute(User user){
            service.execute(() -> userDao.insert(user));
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
        protected User get(int userID) {
            try {
                return service.submit(() -> userDao.findUser(userID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
