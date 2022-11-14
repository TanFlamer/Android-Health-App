package com.example.test;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.room.Database;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.sqlite.db.SupportSQLiteDatabase;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Database(entities = { User.class }, version = 1)
public abstract class UserDatabase extends RoomDatabase {

    private static UserDatabase instance;
    public abstract UserDao getUserDao();

    public static synchronized UserDatabase getInstance(Context context) {
        if (instance == null) {
            instance = Room.databaseBuilder(context.getApplicationContext(),
                            UserDatabase.class, "user_database")
                    .createFromAsset("database/user_database.db")
                    .addCallback(roomCallback)
                    .build();
        }
        return instance;
    }

    private static RoomDatabase.Callback roomCallback = new RoomDatabase.Callback() {
        @Override
        public void onOpen(@NonNull SupportSQLiteDatabase db) {
            super.onOpen(db);
            //new PopulateDbExecutorTask(instance).execute();
        }
    };

    private static class PopulateDbExecutorTask{
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private UserDao userDao;
        PopulateDbExecutorTask(UserDatabase instance) {
            this.userDao = instance.getUserDao();
        }
        protected void execute(){
            List<Test> testList = new ArrayList<>();
            testList.add(new Test("test1", 0));
            testList.add(new Test("test2", 0));
            User user = new User("test2", "0", testList);
            service.execute(() -> userDao.insert(user));
        }
    }
}
