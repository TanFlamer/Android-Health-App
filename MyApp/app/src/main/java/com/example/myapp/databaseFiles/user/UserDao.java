package com.example.myapp.databasefiles.user;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

@Dao
public interface UserDao {

    @Insert
    long insert(User user);

    @Update
    void update(User user);

    @Delete
    void delete(User user);

    @Query("SELECT * FROM Users WHERE userID=:userID")
    User getUser(int userID);

    @Query("SELECT * FROM Users WHERE username=:username")
    User findUser(String username);
}
