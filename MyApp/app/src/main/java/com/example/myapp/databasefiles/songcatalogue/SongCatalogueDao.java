package com.example.myapp.databasefiles.songcatalogue;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import java.util.List;

@Dao
public interface SongCatalogueDao {

    @Insert
    void insert(SongCatalogue songCatalogue); //insert operation for new song catalogue

    @Update
    void update(SongCatalogue songCatalogue); //update operation for existing song catalogue

    @Delete
    void delete(SongCatalogue songCatalogue); //delete operation for existing song catalogue

    @Query("SELECT * FROM SongCatalogue WHERE userID=:userID")
    LiveData<List<SongCatalogue>> getAllSongCatalogue(int userID); //returns live data of all song catalogues belonging to a user
}
