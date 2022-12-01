package com.example.myapp.fragments.sleep.sleepList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;
import com.example.myapp.databasefiles.sleep.Sleep;
import com.example.myapp.subActivities.sleep.SleepDataActivity;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

public class SleepListAdapter extends RecyclerView.Adapter<SleepListAdapter.SleepRecyclerItemViewHolder> {

    Context context;
    List<Sleep> sleepList;
    SleepListViewModel sleepListViewModel;
    HashMap<Sleep, Boolean> visibilityMap;
    HashMap<Sleep, Boolean> buttonMap;

    public SleepListAdapter(Context context, List<Sleep> sleepList, SleepListViewModel sleepListViewModel){
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
        int duration = sleepListViewModel.getDuration(sleep);
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
        sleepListViewModel.sortSleepList(sleepList, data, order);
        for(Sleep sleep : sleepList) visibilityMap.put(sleep, false);
        for(Sleep sleep : sleepList) buttonMap.put(sleep, false);
        notifyDataSetChanged();
    }

    public class SleepRecyclerItemViewHolder extends RecyclerView.ViewHolder {

        TextView titleView, dateView, sleepView, wakeView, durationView;
        LinearLayout layoutVisible, layoutHidden, buttonHidden;
        ImageView clickEdit, clickDelete;

        public SleepRecyclerItemViewHolder(@NonNull View itemView) {
            super(itemView);
            initialiseAll();
        }

        public void initialiseAll(){
            initialiseViewByID();
            initialiseEditButton();
            initialiseDeleteButtons();
            initialiseOnClickListener();
            initialiseOnLongClickListener();
        }

        public void initialiseViewByID(){
            initialiseTextViews();
            initialiseImageButtons();
            initialiseLayouts();
        }

        private void initialiseTextViews(){
            titleView = itemView.findViewById(R.id.sleepTitle);
            dateView = itemView.findViewById(R.id.sleepDate);
            sleepView = itemView.findViewById(R.id.sleepTime);
            wakeView = itemView.findViewById(R.id.wakeTime);
            durationView = itemView.findViewById(R.id.sleepDuration);
        }

        public void initialiseImageButtons(){
            clickEdit = itemView.findViewById(R.id.clickEdit);
            clickDelete = itemView.findViewById(R.id.clickDelete);
        }

        public void initialiseLayouts(){
            layoutVisible = itemView.findViewById(R.id.sleepLayoutVisible);
            layoutHidden = itemView.findViewById(R.id.sleepLayoutHidden);
            buttonHidden = itemView.findViewById(R.id.buttonHidden);
        }

        public void initialiseEditButton(){
            clickEdit.setOnClickListener(v -> {
                long date = sleepList.get(getAdapterPosition()).getDate();
                context.startActivity(sleepListViewModel.sleepEdit(date));
            });
        }

        public void initialiseDeleteButtons(){
            clickDelete.setOnClickListener(view -> {
                Sleep sleep = sleepList.get(getAdapterPosition());
                sleepListViewModel.deleteDialog(context, sleep).show();
            });
        }

        public void initialiseOnClickListener(){
            layoutVisible.setOnClickListener(view -> {
                Sleep sleep = sleepList.get(getAdapterPosition());
                visibilityMap.put(sleep, Boolean.FALSE.equals(visibilityMap.get(sleep)));
                notifyItemChanged(getAdapterPosition());
            });
        }

        public void initialiseOnLongClickListener(){
            layoutVisible.setOnLongClickListener(v -> {
                Sleep sleep = sleepList.get(getAdapterPosition());
                buttonMap.put(sleep, Boolean.FALSE.equals(buttonMap.get(sleep)));
                notifyItemChanged(getAdapterPosition());
                return true;
            });
        }
    }
}
