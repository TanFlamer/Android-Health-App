package com.example.myapp.fragments.sleep.sleepList;

import androidx.recyclerview.widget.DiffUtil;

import com.example.myapp.databaseFiles.sleep.Sleep;

import java.util.List;
import java.util.Objects;

public class SleepRecyclerDiffCallback extends DiffUtil.Callback {

    private final List<Sleep> oldSleepList;
    private final List<Sleep> newSleepList;

    public SleepRecyclerDiffCallback(List<Sleep> oldSleepList, List<Sleep> newSleepList) {
        this.oldSleepList = oldSleepList;
        this.newSleepList = newSleepList;
    }

    @Override
    public int getOldListSize() {
        return oldSleepList.size();
    }

    @Override
    public int getNewListSize() {
        return newSleepList.size();
    }

    @Override
    public boolean areItemsTheSame(int oldItemPosition, int newItemPosition) {
        return Objects.equals(oldSleepList.get(oldItemPosition).getSleepID(), newSleepList.get(newItemPosition).getSleepID());
    }

    @Override
    public boolean areContentsTheSame(int oldItemPosition, int newItemPosition) {
        final Sleep oldSleep = oldSleepList.get(oldItemPosition);
        final Sleep newSleep = newSleepList.get(newItemPosition);
        return oldSleep.getDate().equals(newSleep.getDate()) && oldSleep.getUserID().equals(newSleep.getUserID()) &&
                oldSleep.getSleepTime().equals(newSleep.getSleepTime()) && oldSleep.getWakeTime().equals(newSleep.getWakeTime());
    }
}
