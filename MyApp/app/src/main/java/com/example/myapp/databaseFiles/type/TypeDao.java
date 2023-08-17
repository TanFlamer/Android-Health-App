package com.example.myapp.databaseFiles.type;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import java.util.List;

@Dao
public interface TypeDao {

    @Insert
    void insert(Type type); //insert operation for new sport type

    @Update
    void update(Type type); //update operation for existing sport type

    @Delete
    void delete(Type type); //delete operation for existing sport type

    @Query("SELECT * FROM Types WHERE userID=:userID AND typeName=:typeName")
    Type findType(int userID, String typeName); //check if sport type with specific name exists for a user

    @Query("SELECT * FROM Types WHERE userID=:userID")
    LiveData<List<Type>> getAllTypes(int userID); //returns live data of all sport types belonging to a user
}
