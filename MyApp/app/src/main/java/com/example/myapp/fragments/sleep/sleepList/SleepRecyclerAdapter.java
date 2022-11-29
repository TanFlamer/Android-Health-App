package com.example.myapp.fragments.sleep.sleepList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.recyclerview.widget.DiffUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.sleep.Sleep;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.subActivities.sleep.DataSleep;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

public class SleepRecyclerAdapter extends RecyclerView.Adapter<SleepRecyclerAdapter.SleepRecyclerItemViewHolder> {

    Context context;
    List<Sleep> sleepList;
    SleepListViewModel sleepListViewModel;
    HashMap<Sleep, Boolean> visibilityMap;
    HashMap<Sleep, Boolean> buttonMap;

    public SleepRecyclerAdapter(Context context, List<Sleep> sleepList, SleepListViewModel sleepListViewModel){
        this.context = context;
        this.sleepList = sleepList;
        this.sleepListViewModel = sleepListViewModel;
        visibilityMap = new HashMap<>();
        buttonMap = new HashMap<>();
        for(Sleep sleep : sleepList) visibilityMap.put(sleep, false);
        for(Sleep sleep : sleepList) buttonMap.put(sleep, false);
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
        holder.buttonHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(sleep)) ? View.VISIBLE : View.GONE);
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
        for(Sleep sleep : sleepList) visibilityMap.put(sleep, false);
        for(Sleep sleep : sleepList) buttonMap.put(sleep, false);
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
        LinearLayout layoutVisible, layoutHidden, buttonHidden;
        ImageView clickEdit, clickDelete;

        public SleepRecyclerItemViewHolder(@NonNull View itemView) {
            super(itemView);

            titleView = itemView.findViewById(R.id.sleepTitle);
            dateView = itemView.findViewById(R.id.sleepDate);
            sleepView = itemView.findViewById(R.id.sleepTime);
            wakeView = itemView.findViewById(R.id.wakeTime);
            durationView = itemView.findViewById(R.id.sleepDuration);

            initialiseImageButtons();
            initialiseHiddenLayouts();
        }

        public void initialiseImageButtons(){
            clickEdit = itemView.findViewById(R.id.clickEdit);
            clickEdit.setOnClickListener(v -> {
                Intent intent = new Intent(context, DataSleep.class);
                LocalDate date = Instant.ofEpochMilli(sleepList.get(getAdapterPosition()).getDate()).atZone(ZoneId.systemDefault()).toLocalDate();
                intent.putExtra("year", date.getYear());
                intent.putExtra("month", date.getMonthValue());
                intent.putExtra("day", date.getDayOfMonth());
                context.startActivity(intent);
            });

            clickDelete = itemView.findViewById(R.id.clickDelete);
            clickDelete.setOnClickListener(view -> new AlertDialog.Builder(context)
                    .setTitle("Delete Item")
                    .setMessage("Are you sure you want to delete this item?")
                    .setPositiveButton("Yes", (dialog, which) -> sleepListViewModel.delete(sleepList.get(getAdapterPosition())))
                    .setNegativeButton("No", null)
                    .create()
                    .show());
        }

        public void initialiseHiddenLayouts(){
            layoutVisible = itemView.findViewById(R.id.sleepLayoutVisible);
            layoutHidden = itemView.findViewById(R.id.sleepLayoutHidden);
            buttonHidden = itemView.findViewById(R.id.buttonHidden);

            layoutVisible.setOnClickListener(view -> {
                Sleep sleep = sleepList.get(getAdapterPosition());
                visibilityMap.put(sleep, Boolean.FALSE.equals(visibilityMap.get(sleep)));
                notifyItemChanged(getAdapterPosition());
            });

            layoutVisible.setOnLongClickListener(v -> {
                Sleep sleep = sleepList.get(getAdapterPosition());
                buttonMap.put(sleep, Boolean.FALSE.equals(buttonMap.get(sleep)));
                notifyItemChanged(getAdapterPosition());
                return true;
            });
        }
    }
}
