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
    void insert(SongCatalogue songCatalogue);

    @Update
    void update(SongCatalogue songCatalogue);

    @Delete
    void delete(SongCatalogue songCatalogue);

    @Query("SELECT * FROM SongCatalogue WHERE userID=:userID")
    LiveData<List<SongCatalogue>> getAllSongCatalogue(int userID);
}
