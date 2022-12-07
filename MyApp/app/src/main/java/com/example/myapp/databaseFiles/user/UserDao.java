package com.example.myapp.databasefiles.user;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

@Dao
public interface UserDao {

    @Insert
    long insert(User user); //insert operation for new user

    @Update
    void update(User user); //update operation for existing user

    @Delete
    void delete(User user); //delete operation for existing user

    @Query("SELECT * FROM Users WHERE username=:username")
    User findUser(String username); //check if user with specific name exists
}
