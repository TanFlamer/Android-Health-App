package com.example.myapp.fragments.sport.sportStatistics;

import androidx.recyclerview.widget.DiffUtil;


import com.example.myapp.databasefiles.sport.Sport;

import java.util.List;
import java.util.Objects;

public class SportStatisticsDiffCallback extends DiffUtil.Callback {

    private final List<Sport> oldSportList;
    private final List<Sport> newSportList;

    public SportStatisticsDiffCallback(List<Sport> oldSportList, List<Sport> newSportList) {
        this.oldSportList = oldSportList;
        this.newSportList = newSportList;
    }

    @Override
    public int getOldListSize() {
        return oldSportList.size();
    }

    @Override
    public int getNewListSize() {
        return newSportList.size();
    }

    @Override
    public boolean areItemsTheSame(int oldItemPosition, int newItemPosition) {
        return Objects.equals(oldSportList.get(oldItemPosition).getSportID(), newSportList.get(newItemPosition).getSportID());
    }

    @Override
    public boolean areContentsTheSame(int oldItemPosition, int newItemPosition) {
        final Sport oldSport = oldSportList.get(oldItemPosition);
        final Sport newSport = newSportList.get(newItemPosition);
        return oldSport.getDate().equals(newSport.getDate()) && oldSport.getUserID().equals(newSport.getUserID());
    }
}
