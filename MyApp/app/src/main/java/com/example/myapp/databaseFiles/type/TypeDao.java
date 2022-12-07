package com.example.myapp.databasefiles.type;

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
    void insert(Type type);

    @Update
    void update(Type type);

    @Delete
    void delete(Type type);

    @Query("SELECT * FROM Types WHERE userID=:userID AND typeName=:typeName")
    Type findType(int userID, String typeName);

    @Query("SELECT * FROM Types WHERE userID=:userID")
    LiveData<List<Type>> getAllTypes(int userID);
}
