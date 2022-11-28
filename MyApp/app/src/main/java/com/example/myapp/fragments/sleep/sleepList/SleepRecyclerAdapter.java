package com.example.myapp.fragments.sleep.sleepList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.DiffUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.sleep.Sleep;
import com.example.myapp.databaseFiles.song.Song;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

public class SleepRecyclerAdapter extends RecyclerView.Adapter<SleepRecyclerAdapter.SleepRecyclerItemViewHolder> {

    Context context;
    List<Sleep> sleepList;
    HashMap<Sleep, Boolean> visibilityMap;

    public SleepRecyclerAdapter(Context context, List<Sleep> sleepList){
        this.context = context;
        this.sleepList = sleepList;
        visibilityMap = new HashMap<>();
        for(Sleep sleep : sleepList) visibilityMap.put(sleep, false);
    }

    @NonNull
    @Override
    public SleepRecyclerItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.sleep_recycler_list_item, parent, false);
        return new SleepRecyclerItemViewHolder(view);
    }

    @SuppressLint({"SetTextI18n", "DefaultLocale"})
    @Override
    public void onBindViewHolder(@NonNull SleepRecyclerItemViewHolder holder, int position) {
        Sleep sleep = sleepList.get(position);
        int duration = (sleep.getWakeTime() - sleep.getSleepTime());
        duration += (duration >= 0) ? 0 : 1440;
        holder.titleView.setText(String.valueOf(sleep.getDate()));
        holder.dateView.setText(String.valueOf(sleep.getDate()));
        holder.sleepView.setText(String.valueOf(sleep.getSleepTime()));
        holder.wakeView.setText(String.valueOf(sleep.getWakeTime()));
        holder.durationView.setText(String.format("%02d:%02d", duration / 60, duration % 60));
        holder.layoutHidden.setVisibility(Boolean.TRUE.equals(visibilityMap.get(sleep)) ? View.VISIBLE : View.GONE);
    }

    @Override
    public int getItemCount() {
        return sleepList.size();
    }

    public void updateSleepList(List<Sleep> newSleepList, String data, String order){
        sleepList.clear();
        sleepList.addAll(newSleepList);
        sortSleepList(data, order);
    }

    @SuppressLint("NotifyDataSetChanged")
    public void sortSleepList(String data, String order){
        sleepList.sort(getComparator(data, order));
        visibilityMap.clear();
        for(Sleep sleep : sleepList) visibilityMap.put(sleep, false);
        notifyDataSetChanged();
    }

    public Comparator<Sleep> getComparator(String data, String order){
        Comparator<Sleep> sleepComparator = Comparator.comparingInt(Sleep::getSleepID);
        switch (data) {
            case "Date Added":
                sleepComparator = Comparator.comparingInt(Sleep::getSleepID);
                break;
            case "Sleep Date":
                sleepComparator = Comparator.comparingLong(Sleep::getDate);
                break;
            case "Sleep Time":
                sleepComparator = Comparator.comparingInt(a -> normalisedTime(a.getSleepTime()));
                break;
            case "Wake Time":
                sleepComparator = Comparator.comparingInt(a -> normalisedTime(a.getWakeTime()));
                break;
            case "Sleep Duration":
                sleepComparator = Comparator.comparing(this::getDuration);
                break;
        }
        return order.equals("Ascending") ? sleepComparator : sleepComparator.reversed();
    }

    public int normalisedTime(int time){
        time -= 720;
        if(time < 0) time += 1440;
        return time;
    }

    public int getDuration(Sleep sleep){
        int duration = sleep.getWakeTime() - sleep.getSleepTime();
        return (duration >= 0) ? duration : duration + 1440;
    }

    public class SleepRecyclerItemViewHolder extends RecyclerView.ViewHolder {

        TextView titleView, dateView, sleepView, wakeView, durationView;
        LinearLayout layoutVisible, layoutHidden;

        public SleepRecyclerItemViewHolder(@NonNull View itemView) {
            super(itemView);

            titleView = itemView.findViewById(R.id.sleepTitle);
            dateView = itemView.findViewById(R.id.sleepDate);
            sleepView = itemView.findViewById(R.id.sleepTime);
            wakeView = itemView.findViewById(R.id.wakeTime);
            durationView = itemView.findViewById(R.id.sleepDuration);

            layoutVisible = itemView.findViewById(R.id.sleepLayoutVisible);
            layoutHidden = itemView.findViewById(R.id.sleepLayoutHidden);

            layoutVisible.setOnClickListener(view -> {
                Sleep sleep = sleepList.get(getAdapterPosition());
                visibilityMap.put(sleep, Boolean.FALSE.equals(visibilityMap.get(sleep)));
                notifyItemChanged(getAdapterPosition());
            });
        }
    }
}
